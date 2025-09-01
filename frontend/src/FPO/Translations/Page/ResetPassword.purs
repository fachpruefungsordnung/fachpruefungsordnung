module FPO.Translations.Page.ResetPassword where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type PasswordResetLabels =
  ( "rp_ConfirmationCode"
      ::: "rp_Header"
      ::: "rp_ifYouNeedANewCode"
      ::: "rp_InputCode"
      ::: "rp_NoEmail"
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
  , rp_ifYouNeedANewCode:
      "If you need a new code, click on the mail icon and enter your email."
  , rp_InputCode: "Input Code here"
  , rp_NoEmail: "No email address provided."
  , rp_NoMatch: "The passwords do not match."
  , rp_PasswordConfirm: "Repeat new password"
  , rp_PasswordNew: "New password"
  , rp_RequestCode: "Request Code"
  }

dePasswordReset :: Translation PasswordResetLabels
dePasswordReset = fromRecord
  { rp_ConfirmationCode: "Bestätigungscode"
  , rp_Header: "Passwort zurücksetzen"
  , rp_ifYouNeedANewCode:
      "Wenn Sie einen neuen Code benötigen, klicken Sie auf das Mail-Symbol und geben Sie Ihre E-Mail-Adresse ein."
  , rp_InputCode: "Code hier eingeben"
  , rp_NoEmail: "Keine E-Mail-Adresse angegeben."
  , rp_NoMatch: "Die Passwörter stimmen nicht überein."
  , rp_PasswordConfirm: "Neues Passwort wiederholen"
  , rp_PasswordNew: "Neues Passwort"
  , rp_RequestCode: "Code anfordern"
  }
