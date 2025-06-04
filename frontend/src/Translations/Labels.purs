module Translations.Labels where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

-- | All kinds of abstract labels representing UI texts,
-- | detached from the actual language selection.
-- |
-- | Symbols MUST be in alphabetic order.
-- | Because of this constraint, it's sensible to use
-- | appropriate prefixes for strongly related labels.
type Labels =
  ( "email"
      ::: "emailAddress"
      ::: "home"
      ::: "loginSuccessful"
      ::: "password"
      ::: "passwordForgotten"
      ::: "profile"
      ::: "role"

      -- | Reset Password Page
      ::: "rpConfirmationCode"
      ::: "rpHeader"
      ::: "rpInputCode"
      ::: "rpNoMatch"
      ::: "rpPasswordConfirm"
      ::: "rpPasswordNew"
      ::: "rpRequestCode"

      ::: "submit"
      ::: "userData"
      ::: "userName"
      ::: SNil
  )

en :: Translation Labels
en = fromRecord
  { email: "Email"
  , emailAddress: "Email address"
  , home: "Home"
  , loginSuccessful: "Login successful"
  , password: "Password"
  , passwordForgotten: "Forgot password?"
  , profile: "Profile"
  , role: "Role"

  , rpConfirmationCode: "Confirmation Code"
  , rpHeader: "Reset Password"
  , rpInputCode: "Input Code here"
  , rpNoMatch: "The passwords do not match."
  , rpPasswordConfirm: "Repeat new password"
  , rpPasswordNew: "New password"
  , rpRequestCode: "Request Code"

  , submit: "Submit"
  , userData: "User data"
  , userName: "User name"
  }

de :: Translation Labels
de = fromRecord
  { email: "E-Mail"
  , emailAddress: "E-Mail-Adresse"
  , home: "Start"
  , loginSuccessful: "Login erfolgreich"
  , password: "Passwort"
  , passwordForgotten: "Passwort vergessen?"
  , profile: "Profil"
  , role: "Rolle"

  , rpConfirmationCode: "Bestätigungscode"
  , rpHeader: "Passwort zurücksetzen"
  , rpInputCode: "Code hier eingeben"
  , rpNoMatch: "Die Passwörter stimmen nicht überein."
  , rpPasswordConfirm: "Neues Passwort wiederholen"
  , rpPasswordNew: "Neues Passwort"
  , rpRequestCode: "Code anfordern"

  , submit: "Absenden"
  , userData: "Benutzerdaten"
  , userName: "Benutzername"
  }
