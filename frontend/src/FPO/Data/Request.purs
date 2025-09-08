-- | This module provides functions for making HTTP requests, including GET and POST requests.
-- | It supports various response formats such as String, JSON, Document, and Blob.
-- | The functions use the Affjax library to handle asynchronous HTTP requests.

module FPO.Data.Request
  ( LoadState(..)
  , addGroup
  , changeRole
  , createNewDocument
  , deleteIgnore
  , fromLoading
  , getAuthorizedUser
  , getBlob
  , getDocument
  , getDocumentHeader
  , getDocumentsQueryFromURL
  , getFromJSONEndpoint
  , getGroup
  , getGroups
  , getIgnore
  , getJson
  , getString
  , getTextElemHistoryAll
  , getUser
  , getUserDocuments
  , getUserGroups
  , getUserWithId
  , patchJson
  , patchString
  , postBlob
  , postBlobOrError
  , postDocument
  , postIgnore
  , postIgnoreOrError
  , postJson
  , postRenderHtml
  , postString
  , putIgnore
  , putJson
  , removeUser
  ) where

import Prelude

import Affjax (AffjaxDriver, Error, Request, Response, request)
import Affjax.RequestBody (json) as RequestBody
import Affjax.RequestHeader (RequestHeader(RequestHeader))
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat (blob, document, ignore, json, string) as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson, fromString)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FPO.Data.AppError (AppError(..), printAjaxError)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..))
import FPO.Data.Store as Store
import FPO.Dto.CreateDocumentDto (NewDocumentCreateDto)
import FPO.Dto.DocumentDto.DocDate as DD
import FPO.Dto.DocumentDto.DocumentHeader as DH
import FPO.Dto.DocumentDto.FullDocument (decodeFullDocument)
import FPO.Dto.DocumentDto.FullDocument as FD
import FPO.Dto.DocumentDto.Query as DQ
import FPO.Dto.DocumentDto.TextElement as TE
import FPO.Dto.GroupDto
  ( GroupCreate
  , GroupDto
  , GroupID
  , GroupOverview
  , toGroupOverview
  )
import FPO.Dto.UserDto
  ( FullUserDto
  , UserID
  , getAllAdminRoles
  , isAdminOf
  , isUserSuperadmin
  )
import FPO.Dto.UserRoleDto (Role)
import FPO.Translations.Translator (fromFpoTranslator)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Simple.I18n.Translator (label, translate)
import Web.DOM.Document (Document)
import Web.File.Blob (Blob)

-- | Foreign imports
foreign import driver :: AffjaxDriver
foreign import getCookieEff :: String -> Effect String

-- | Helper-Funktion zum Erstellen einer FPO-Request mit XSRF-Token
defaultFpoRequest
  :: forall a. ResponseFormat a -> String -> Method -> Effect (Request a)
defaultFpoRequest responseFormat url method = do
  xsrfToken <- getCookieEff "XSRF-TOKEN"
  pure
    { method: Left method
    , url
    , headers: [ RequestHeader "X-XSRF-TOKEN" xsrfToken ]
    , content: Nothing
    , username: Nothing
    , password: Nothing
    , withCredentials: false
    , responseFormat
    , timeout: Nothing
    }

-- | High-level requests ---------------------------------------------------

-- | State of an asynchronous load operation.
-- | It can either be in a loading state or have successfully loaded data.
data LoadState a = Loading | Loaded a

-- | Extracts the loaded value from a LoadState, providing a default if not loaded.
fromLoading :: forall a. LoadState a -> a -> a
fromLoading Loading a = a
fromLoading (Loaded a) _ = a

derive instance eqLoadState :: Eq a => Eq (LoadState a)

-- | Generic Error Handling Functions ----------------------------------------

-- | Generic wrapper for any Aff request that returns Either Error (Response a)
-- | Converts Ajax errors and HTTP status codes to AppError
handleRequest'
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String -- URL for error context
  -> Aff (Either Error (Response a))
  -> H.HalogenM st act slots msg m (Either AppError a)
handleRequest' url requestAction = do
  store <- getStore
  response <- H.liftAff requestAction
  case response of
    Left err -> do
      pure $ Left $ NetworkError $ printAjaxError
        ( translate (label :: _ "error_connectionFailed")
            (fromFpoTranslator store.translator)
        )
        err
    Right { body, status } -> do
      case status of
        StatusCode 401 -> do
          let
            errorMessage =
              if url == "/login" then
                ( translate (label :: _ "error_invalidCredentials")
                    (fromFpoTranslator store.translator)
                )
              else
                ( translate (label :: _ "error_sessionExpired")
                    (fromFpoTranslator store.translator)
                )
            appError = AuthError errorMessage
          updateStore $ Store.SetLoginRedirect store.currentRoute
          handleAppError appError
          pure $ Left appError
        StatusCode 403 -> do
          handleAppError AccessDeniedError
          pure $ Left AccessDeniedError
        StatusCode 404 -> do
          handleAppError (NotFoundError url)
          pure $ Left $ NotFoundError url
        StatusCode 405 -> do
          handleAppError (MethodNotAllowedError url "Unknown")
          pure $ Left $ MethodNotAllowedError url "Unknown"
        StatusCode code | code >= 500 && code < 600 -> do
          handleAppError (ServerError $ "Server error (status: " <> show code <> ")")
          pure $ Left $ ServerError $ "Server error (status: " <> show code <> ")"
        StatusCode 200 ->
          pure $ Right body
        StatusCode 201 ->
          pure $ Right body
        StatusCode 204 ->
          pure $ Right body
        StatusCode code -> do
          handleAppError (ServerError $ "Unexpected status code: " <> show code)
          pure $ Left $ ServerError $ "Unexpected status code: " <> show code

-- | Helper function to handle app errors.
handleAppError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => AppError
  -> H.HalogenM st act slots msg m Unit
handleAppError err = do
  s <- getStore
  when s.handleRequestError $ do
    updateStore $ Store.AddError err
    case err of
      NetworkError _ -> pure unit -- Let component handle this
      AuthError _ -> navigate Login
      NotFoundError _ -> navigate Page404
      ServerError _ -> pure unit -- Let component handle this
      DataError _ -> pure unit -- Let component handle this
      AccessDeniedError -> pure unit -- Let component handle this
      MethodNotAllowedError _ _ -> pure unit -- Let component handle this

-- | Wrapper specifically for JSON responses with decode step
handleJsonRequest'
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Aff (Either Error (Response Json))
  -> H.HalogenM st act slots msg m (Either AppError a)
handleJsonRequest' decode url requestAction = do
  result <- handleRequest' url requestAction
  case result of
    Left appError -> pure $ Left appError
    Right json -> do
      case decode json of
        Left err -> do
          liftEffect $ log $ "JSON decode error: " <> show err
          pure $ Left $ DataError $ "Invalid data format: " <> show err
        Right val ->
          pure $ Right val

-- | Helper for requests that don't return meaningful body (Unit responses)
handleUnitRequest
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Aff (Either Error (Response Unit))
  -> H.HalogenM st act slots msg m (Either AppError Unit)
handleUnitRequest = handleRequest'

getFromJSONEndpoint
  :: forall a. (Json -> Either JsonDecodeError a) -> String -> Aff (Maybe a)
getFromJSONEndpoint decode url = do
  response <- getJson' url
  case response of
    Left _ ->
      pure Nothing
    Right res -> do
      case decode (res.body) of
        Left err -> do
          liftEffect $ log $ "Error Decoding: " <> show err
          pure Nothing
        Right val -> do
          pure $ Just val

-- | Simplified Error-Handling HTTP Methods ----------------------------------

-- | Error-handling versions of basic HTTP methods
getString
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError String)
getString url = handleRequest' url (getString' url)

getJson
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> H.HalogenM st act slots msg m (Either AppError a)
getJson decode url = handleJsonRequest' decode url (getJson' url)

getBlob
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Blob)
getBlob url = handleRequest' url (getBlob' url)

getDocument
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Document)
getDocument url = handleRequest' url (getDocument' url)

getIgnore
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Unit)
getIgnore url = handleUnitRequest url (getIgnore' url)

postIgnore
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Unit)
postIgnore url body = handleUnitRequest url (postIgnore' url body)

postJson
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
postJson decode url body = handleJsonRequest' decode url
  (postJson' url body)

postString
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError String)
postString url body = handleRequest' url (postString' url body)

postBlob
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Blob)
postBlob url body = handleRequest' url (postBlob' url body)

postDocument
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Document)
postDocument url body = handleRequest' url (postDocument' url body)

putJson
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
putJson decode url body = handleJsonRequest' decode url
  (putJson' url body)

putIgnore
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError Unit)
putIgnore url body = handleUnitRequest url (putIgnore' url body)

patchJson
  :: forall a st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => (Json -> Either JsonDecodeError a)
  -> String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError a)
patchJson decode url body = handleJsonRequest' decode url
  (patchJson' url body)

patchString
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError String)
patchString url body = handleRequest' url (patchString' url body)

deleteIgnore
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError Unit)
deleteIgnore url = handleUnitRequest url (deleteIgnore' url)

-- | Fetches the authorized user for a specific group.
-- | Returns Nothing if the user is not existing or not authorized.
getAuthorizedUser
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupID
  -> H.HalogenM st act slots msg m (Either AppError (Maybe FullUserDto))
getAuthorizedUser groupID = do
  userResult <- getUser
  case userResult of
    Left appError -> pure $ Left appError
    Right user -> do
      if (isUserSuperadmin user || user `isAdminOf` groupID) then pure $ Right $ Just
        user
      else pure $ Right Nothing

getGroups
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError (Array GroupOverview))
getGroups = getJson (decodeArray decodeJson) "/groups"

getUserGroups
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError (Array GroupOverview))
getUserGroups = do
  user <- getUser
  case user of
    Left err -> pure $ Left err
    Right u | isUserSuperadmin u -> getGroups
    Right u -> pure $ Right $ map toGroupOverview $ getAllAdminRoles u

-- | Error-handling versions of domain-specific functions
getUser
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => H.HalogenM st act slots msg m (Either AppError FullUserDto)
getUser = getJson decodeJson "/me"

getUserWithId
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError FullUserDto)
getUserWithId userId = getJson decodeJson ("/users/" <> userId)

getGroup
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupID
  -> H.HalogenM st act slots msg m (Either AppError GroupDto)
getGroup groupID = getJson decodeJson ("/groups/" <> show groupID)

changeRole
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupID
  -> UserID
  -> Role
  -> H.HalogenM st act slots msg m (Either AppError Unit)
changeRole groupID userID role =
  putIgnore ("/roles/" <> show groupID <> "/" <> userID) (encodeJson role)

removeUser
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupID
  -> UserID
  -> H.HalogenM st act slots msg m (Either AppError Unit)
removeUser groupID userID =
  deleteIgnore ("/roles/" <> show groupID <> "/" <> userID)

getDocumentHeader
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => DH.DocumentID
  -> H.HalogenM st act slots msg m (Either AppError DH.DocumentHeader)
getDocumentHeader docID = getJson decodeJson ("/docs/" <> show docID)

createNewDocument
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => NewDocumentCreateDto
  -> H.HalogenM st act slots msg m (Either AppError FD.FullDocument)
createNewDocument dto = postJson decodeFullDocument "/docs" (encodeJson dto)

getDocumentsQueryFromURL
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError DQ.DocumentQuery)
getDocumentsQueryFromURL url = getJson decodeJson url

getTextElemHistoryAll
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => DH.DocumentID
  -> TE.TextElementID
  -> DD.DocDate
  -> H.HalogenM st act slots msg m (Either AppError TE.FullTextElementHistory)
getTextElemHistoryAll dID tID date =
  getJson
    decodeJson
    ( "/docs/" <> show dID <> "/text/" <> show tID <> "/history?before="
        <> DD.toStringFormat date
    )

getUserDocuments
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => UserID
  -> H.HalogenM st act slots msg m (Either AppError (Array DH.DocumentHeader))
getUserDocuments userID = do
  result <- getDocumentsQueryFromURL $ "/docs?user=" <> userID
  case result of
    Left err -> pure $ Left err
    Right dq -> pure $ Right $ DQ.getDocuments dq

addGroup
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => GroupCreate
  -> H.HalogenM st act slots msg m (Either AppError GroupID)
addGroup group = postJson decodeJson "/groups" (encodeJson group)

postRenderHtml
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> H.HalogenM st act slots msg m (Either AppError String)
postRenderHtml content = do
  result <- handleRequest' "/docs/render/html" (postRenderHtml' content)
  pure result

-- | Makes a POST request that can handle both Blob success and String error responses
-- | Currently used for PDF generation to be able to display the internal error case.
postBlobOrError
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError (Either String Blob))
postBlobOrError url body = do
  -- First try as blob
  blobResult <- handleRequest' url (postBlob' url body)
  case blobResult of
    Right blob -> pure $ Right $ Right blob
    Left _ -> do
      -- If blob failed, try as string (likely an error message)
      stringResult <- H.liftAff $ postString' url body
      case stringResult of
        Right { body: errMsg, status } -> case status of
          StatusCode 400 -> pure $ Right $ Left errMsg
          _ -> pure $ Right $ Left "Unknown error occurred"
        Left _ -> pure $ Left (ServerError "Error creating Blob.")

postIgnoreOrError
  :: forall st act slots msg m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Navigate m
  => String
  -> Json
  -> H.HalogenM st act slots msg m (Either AppError (Either String Unit))
postIgnoreOrError url body = do
  -- First try as blob
  ignoreResult <- handleRequest' url (postIgnore' url body)
  case ignoreResult of
    Right blob -> pure $ Right $ Right blob
    Left _ -> do
      -- If blob failed, try as string (likely an error message)
      stringResult <- H.liftAff $ postString' url body
      case stringResult of
        Right { body: errMsg, status } -> case status of
          StatusCode 400 -> pure $ Right $ Left errMsg
          _ -> pure $ Right $ Left "Unknown error occurred"
        Left _ -> pure $ Left (ServerError "Error while requesting.")

-- | PUT-Requests ----------------------------------------------------------
putJson' :: String -> Json -> Aff (Either Error (Response Json))
putJson' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) PUT
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

putIgnore' :: String -> Json -> Aff (Either Error (Response Unit))
putIgnore' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) PUT
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | GET-Requests ----------------------------------------------------------

-- | Makes a GET request and expects a String response.
getString' :: String -> Aff (Either Error (Response String))
getString' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a JSON response.
getJson' :: String -> Aff (Either Error (Response Json))
getJson' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Document response.
getDocument' :: String -> Aff (Either Error (Response Document))
getDocument' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.document ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Blob response.
getBlob' :: String -> Aff (Either Error (Response Blob))
getBlob' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.blob ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Null response.
getIgnore' :: String -> Aff (Either Error (Response Unit))
getIgnore' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | POST-Requests ---------------------------------------------------------

-- | Makes a POST request with a JSON body and expects a String response.
postString' :: String -> Json -> Aff (Either Error (Response String))
postString' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request to render the content of a paragraph with a String body and expects a String response.
postRenderHtml' :: String -> Aff (Either Error (Response String))
postRenderHtml' content = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string
    ("/api/docs/render/html")
    POST
  let request' = fpoRequest { content = Just (RequestBody.json $ fromString content) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a JSON response.
postJson' :: String -> Json -> Aff (Either Error (Response Json))
postJson' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a Document response.
postDocument' :: String -> Json -> Aff (Either Error (Response Document))
postDocument' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.document ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a Blob response.
postBlob' :: String -> Json -> Aff (Either Error (Response Blob))
postBlob' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.blob ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request and expects a Null response.
postIgnore' :: String -> Json -> Aff (Either Error (Response Unit))
postIgnore' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | PATCH Requests -----------------------------------------------------

-- | Makes a PATCH request with a JSON body and expects a String response.
patchString' :: String -> Json -> Aff (Either Error (Response String))
patchString' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) PATCH
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a PATCH request with a JSON body and expects a JSON response.
patchJson' :: String -> Json -> Aff (Either Error (Response Json))
patchJson' url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) PATCH
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | DELETE Requests -------------------------------------------------------

-- | Makes a DELETE request and expects a Null response.
deleteIgnore' :: String -> Aff (Either Error (Response Unit))
deleteIgnore' url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) DELETE
  liftAff $ request driver fpoRequest
