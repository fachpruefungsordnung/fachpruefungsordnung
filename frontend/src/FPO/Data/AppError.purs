module FPO.Data.AppError where

import Prelude

import Affjax (Error(..))
import Effect.Aff (message)
import Effect.Aff.Class (class MonadAff)
import FPO.Data.Navigate (class Navigate, navigate)
import FPO.Data.Route (Route(..))
import Foreign (renderForeignError)
import Halogen as H

-- Add this after your imports
data AppError
  = NetworkError String
  | AuthError
  | NotFoundError String
  | ServerError String
  | DataError String
  | AccessDeniedError
  | MethodNotAllowedError String String

type ErrorId = Int
type AppErrorWithId = { errorId :: ErrorId, error :: AppError }

derive instance Eq AppError
instance Show AppError where
  show = case _ of
    NetworkError msg -> "NetworkError: " <> msg
    AuthError -> "AuthError"
    NotFoundError resource -> "NotFoundError: " <> resource
    ServerError msg -> "ServerError: " <> msg
    DataError msg -> "DataError: " <> msg
    AccessDeniedError -> "AccessDeniedError"
    MethodNotAllowedError resource method -> "MethodNotAllowedError: " <> resource
      <> " (method: "
      <> method
      <> ")"

-- Helper function to handle app errors
handleAppError
  :: forall st act slots msg m
   . MonadAff m
  => Navigate m
  => AppError
  -> H.HalogenM st act slots msg m Unit
handleAppError = case _ of
  NetworkError _ -> pure unit -- Let component handle this
  AuthError -> navigate Login
  NotFoundError _ -> navigate Page404
  ServerError _ -> pure unit -- Let component handle this
  DataError _ -> pure unit -- Let component handle this
  AccessDeniedError -> pure unit -- Let component handle this
  MethodNotAllowedError _ _ -> pure unit -- Let component handle this

-- | Prints an error message based on the type of error.
-- | The error message is prefixed with the provided string.
printAjaxError :: String -> Error -> String
printAjaxError str = case _ of
  RequestContentError err ->
    str <> ": " <> err
  ResponseBodyError err _ ->
    str <> ": " <> renderForeignError err
  TimeoutError ->
    str <> ": timeout"
  RequestFailedError ->
    str <> ": request failed"
  XHROtherError err ->
    str <> ": " <> message err
