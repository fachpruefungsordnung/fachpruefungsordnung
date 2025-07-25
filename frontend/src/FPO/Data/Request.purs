-- | This module provides functions for making HTTP requests, including GET and POST requests.
-- | It supports various response formats such as String, JSON, Document, and Blob.
-- | The functions use the Affjax library to handle asynchronous HTTP requests.

module FPO.Data.Request where

import Prelude

import Affjax (AffjaxDriver, Error(..), Request, Response, request)
import Affjax.RequestBody (json) as RequestBody
import Affjax.RequestHeader (RequestHeader(RequestHeader))
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat (blob, document, ignore, json, string) as AXRF
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (JsonDecodeError, decodeJson, encodeJson)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Decoders (decodeArray)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Exn
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import FPO.Dto.DocumentDto (DocumentHeader, DocumentHeaderPlusPermission, DocumentID)
import FPO.Dto.GroupDto (GroupCreate, GroupOverview)
import FPO.Dto.UserDto (User, decodeUser)
import Foreign (renderForeignError)
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

-- | Prints an error message based on the type of error.
-- | The error message is prefixed with the provided string.
printError :: String -> Error -> String
printError str = case _ of
  RequestContentError err ->
    str <> ": " <> err
  ResponseBodyError err _ ->
    str <> ": " <> renderForeignError err
  TimeoutError ->
    str <> ": timeout"
  RequestFailedError ->
    str <> ": request failed"
  XHROtherError err ->
    str <> ": " <> Exn.message err

-- | High-level requests ---------------------------------------------------

-- | State of an asynchronous load operation.
-- | It can either be in a loading state or have successfully loaded data.
data LoadState a = Loading | Loaded a

getFromJSONEndpoint
  :: forall a. (Json -> Either JsonDecodeError a) -> String -> Aff (Maybe a)
getFromJSONEndpoint decode url = do
  response <- getJson url
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

-- | Fetches the current user from the server.
getUser :: Aff (Maybe User)
getUser = getFromJSONEndpoint decodeUser "/me"

-- | Fetches the groups of the current user from the server.
getGroups :: Aff (Maybe (Array GroupOverview))
getGroups = getFromJSONEndpoint (decodeArray decodeJson) "/groups"

-- | Fetches the document header for a given document ID.
getDocumentHeader :: DocumentID -> Aff (Maybe DocumentHeader)
getDocumentHeader docID = getFromJSONEndpoint decodeJson ("/documents/" <> show docID)

getDocumentsFromURL :: String -> Aff (Maybe (Array DocumentHeader))
getDocumentsFromURL url = getFromJSONEndpoint (decodeArray decodeJson) url

getDocumentsFromURLWithPermission
  :: String -> Aff (Maybe (Array DocumentHeaderPlusPermission))
getDocumentsFromURLWithPermission url = getFromJSONEndpoint
  (decodeArray decodeJson)
  url

addGroup :: GroupCreate -> Aff (Either Error (Response Json))
addGroup group = postJson "/groups" (encodeJson group)

-- | GET-Requests ----------------------------------------------------------

-- | Makes a GET request and expects a String response.
getString :: String -> Aff (Either Error (Response String))
getString url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a JSON response.
getJson :: String -> Aff (Either Error (Response Json))
getJson url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Document response.
getDocument :: String -> Aff (Either Error (Response Document))
getDocument url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.document ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Blob response.
getBlob :: String -> Aff (Either Error (Response Blob))
getBlob url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.blob ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | Makes a GET request and expects a Null response.
getIgnore :: String -> Aff (Either Error (Response Unit))
getIgnore url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) GET
  liftAff $ request driver fpoRequest

-- | POST-Requests ---------------------------------------------------------

-- | Makes a POST request with a JSON body and expects a String response.
postString :: String -> Json -> Aff (Either Error (Response String))
postString url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.string ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a JSON response.
postJson :: String -> Json -> Aff (Either Error (Response Json))
postJson url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.json ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a Document response.
postDocument :: String -> Json -> Aff (Either Error (Response Document))
postDocument url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.document ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | Makes a POST request with a JSON body and expects a Blob response.
postBlob :: String -> Json -> Aff (Either Error (Response Blob))
postBlob url body = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.blob ("/api" <> url) POST
  let request' = fpoRequest { content = Just (RequestBody.json body) }
  liftAff $ request driver request'

-- | DELETE Requests -------------------------------------------------------

-- | Makes a DELETE request and expects a Null response.
deleteIgnore :: String -> Aff (Either Error (Response Unit))
deleteIgnore url = do
  fpoRequest <- liftEffect $ defaultFpoRequest AXRF.ignore ("/api" <> url) DELETE
  liftAff $ request driver fpoRequest

-- | Auxiliary Functions -----------------------------------------------------

-- | Extracts the status code from a response.
getStatusCode :: forall a. Response a -> Int
getStatusCode response = extract $ response.status
  where
  extract (StatusCode code) = code
