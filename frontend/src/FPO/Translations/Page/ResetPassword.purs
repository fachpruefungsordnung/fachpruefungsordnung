module FPO.Translations.Page.ResetPassword where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type PasswordResetLabels =
  ( "rp_Header"
      ::: "rp_NoEmail"
      ::: "rp_NoMatch"
      ::: "rp_PasswordConfirm"
      ::: "rp_PasswordNew"
      ::: "rp_linkSentBodyAfter"
      ::: "rp_linkSentBodyBefore"
      ::: "rp_linkSentTitle"
      ::: "rp_newPasswordHint"
      ::: "rp_requestLinkHint"
      ::: "rp_sendResetLink"
      ::: "rp_setNewPassword"
      ::: SNil
  )

enPasswordReset :: Translation PasswordResetLabels
enPasswordReset = fromRecord
  { rp_Header: "Reset Password"
  , rp_NoEmail: "No email address provided."
  , rp_NoMatch: "The passwords do not match."
  , rp_PasswordConfirm: "Repeat new password"
  , rp_PasswordNew: "New password"
  , rp_linkSentBodyAfter:
      "you will receive an email with instructions to reset your password."
  , rp_linkSentBodyBefore: "If an account exists for"
  , rp_linkSentTitle: "Check your inbox"
  , rp_newPasswordHint: "Choose a new password for your account."
  , rp_requestLinkHint:
      "Enter your email address and we will send you a link to reset your password."
  , rp_sendResetLink: "Send reset link"
  , rp_setNewPassword: "Set new password"
  }

dePasswordReset :: Translation PasswordResetLabels
dePasswordReset = fromRecord
  { rp_Header: "Passwort zurücksetzen"
  , rp_NoEmail: "Keine E-Mail-Adresse angegeben."
  , rp_NoMatch: "Die Passwörter stimmen nicht überein."
  , rp_PasswordConfirm: "Neues Passwort wiederholen"
  , rp_PasswordNew: "Neues Passwort"
  , rp_linkSentBodyAfter:
      "existiert, erhalten Sie eine E-Mail mit Anweisungen zum Zurücksetzen Ihres Passworts."
  , rp_linkSentBodyBefore: "Falls ein Konto für"
  , rp_linkSentTitle: "Posteingang prüfen"
  , rp_newPasswordHint: "Wählen Sie ein neues Passwort für Ihr Konto."
  , rp_requestLinkHint:
      "Geben Sie Ihre E-Mail-Adresse ein und wir senden Ihnen einen Link zum Zurücksetzen Ihres Passworts."
  , rp_sendResetLink: "Link senden"
  , rp_setNewPassword: "Neues Passwort setzen"
  }
