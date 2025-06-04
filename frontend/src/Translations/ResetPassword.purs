module Translations.ResetPassword where

import Prelude

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type PasswordResetLabels =
  ( "rp_ConfirmationCode"
      ::: "rp_Header"
      ::: "rp_InputCode"
      ::: "rp_NoMatch"
      ::: "rp_PasswordConfirm"
      ::: "rp_PasswordNew"
      ::: "rp_RequestCode"
      ::: SNil
  )

enPasswordReset :: Translation PasswordResetLabels
enPasswordReset = fromRecord
  { rp_ConfirmationCode: "Confirmation Code"
  , rp_Header: "Reset Password"
  , rp_InputCode: "Input Code here"
  , rp_NoMatch: "The passwords do not match."
  , rp_PasswordConfirm: "Repeat new password"
  , rp_PasswordNew: "New password"
  , rp_RequestCode: "Request Code"
  }

dePasswordReset :: Translation PasswordResetLabels
dePasswordReset = fromRecord
  { rp_ConfirmationCode: "Bestätigungscode"
  , rp_Header: "Passwort zurücksetzen"
  , rp_InputCode: "Code hier eingeben"
  , rp_NoMatch: "Die Passwörter stimmen nicht überein."
  , rp_PasswordConfirm: "Neues Passwort wiederholen"
  , rp_PasswordNew: "Neues Passwort"
  , rp_RequestCode: "Code anfordern"
  }
