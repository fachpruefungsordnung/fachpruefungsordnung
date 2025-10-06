module FPO.Translations.Errors
  ( deErrors
  , enErrors
  ) where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type ErrorLabels =
  ( "error_accessDeniedError"
      ::: "error_authError"
      ::: "error_conflictError"
      ::: "error_conflictError_groups"
      ::: "error_connectionFailed"
      ::: "error_dataError"
      ::: "error_invalidCredentials"
      ::: "error_methodNotAllowedError"
      ::: "error_networkError"
      ::: "error_notFoundError"
      ::: "error_serverError"
      ::: "error_sessionExpired"
      ::: SNil
  )

enErrors :: Translation ErrorLabels
enErrors = fromRecord
  { error_accessDeniedError: "Access denied"
  , error_authError: "Authentication error: "
  , error_conflictError: "Conflict error"
  , error_conflictError_groups: "A group with this name already exists"
  , error_connectionFailed: "Connection failed"
  , error_dataError: "Data error: "
  , error_methodNotAllowedError: "Method not allowed: "
  , error_invalidCredentials: "Invalid credentials"
  , error_networkError: "Network error: "
  , error_notFoundError: "Not found: "
  , error_serverError: "Server error: "
  , error_sessionExpired: "Session expired"
  }

deErrors :: Translation ErrorLabels
deErrors = fromRecord
  { error_accessDeniedError: "Zugriff verweigert"
  , error_authError: "Authentifizierungsfehler: "
  , error_conflictError: "Konfliktfehler"
  , error_conflictError_groups: "Eine Gruppe mit diesem Namen existiert bereits"
  , error_connectionFailed: "Verbindungsfehler"
  , error_dataError: "Datenfehler: "
  , error_methodNotAllowedError: "Methode nicht erlaubt: "
  , error_invalidCredentials: "Ungültige Anmeldedaten"
  , error_networkError: "Netzwerkfehler: "
  , error_notFoundError: "Nicht gefunden: "
  , error_serverError: "Serverfehler: "
  , error_sessionExpired: "Sitzung abgelaufen"
  }
