module FPO.Data.AppError where

import Prelude

import Affjax (Error(..))
import Effect.Aff (message)
import FPO.Translations.Labels (Labels)
import Foreign (renderForeignError)
import Simple.I18n.Translator (Translator, label, translate)

-- Add this after your imports
data AppError
  = NetworkError String
  | AuthError String
  | NotFoundError String
  | ServerError String
  | DataError String
  | AccessDeniedError
  | MethodNotAllowedError String String
  | ConflictError String

type ErrorId = Int
type AppErrorWithId = { errorId :: ErrorId, error :: AppError }

derive instance Eq AppError
instance Show AppError where
  show = case _ of
    NetworkError msg -> "NetworkError: " <> msg
    AuthError msg -> "AuthError: " <> msg
    NotFoundError resource -> "NotFoundError: " <> resource
    ServerError msg -> "ServerError: " <> msg
    DataError msg -> "DataError: " <> msg
    AccessDeniedError -> "AccessDeniedError"
    MethodNotAllowedError resource method -> "MethodNotAllowedError: " <> resource
      <> " (method: "
      <> method
      <> ")"
    ConflictError _ -> "ConflictError"

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

showToastError :: AppError -> Translator Labels -> String
showToastError err translator = case err of
  NetworkError msg -> (translate (label :: _ "error_networkError") translator) <> msg
  AuthError msg -> (translate (label :: _ "error_authError") translator) <> msg
  NotFoundError resource -> (translate (label :: _ "error_notFoundError") translator)
    <> resource
  ServerError msg -> (translate (label :: _ "error_serverError") translator) <> msg
  DataError msg -> (translate (label :: _ "error_dataError") translator) <> msg
  AccessDeniedError -> translate (label :: _ "error_accessDeniedError") translator
  MethodNotAllowedError resource method ->
    (translate (label :: _ "error_methodNotAllowedError") translator) <> resource
      <> " (method: "
      <> method
      <> ")"
  ConflictError url -> (translate (label :: _ "error_conflictError") translator) <>
    ( if url == "/groups" then
        ((": ") <> (translate (label :: _ "error_conflictError_groups") translator))
      else ""
    )
