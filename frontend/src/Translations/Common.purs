module Translations.Common where

import Prelude

import Record.Extra (SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type CommonLabels =
  ( "email" ::: "emailAddress" ::: "home" ::: "password" ::: "profile" ::: "role"
      ::: "submit"
      ::: "userData"
      ::: "userName"
      ::: SNil
  )

enCommon :: Translation CommonLabels
enCommon = fromRecord
  { email: "Email"
  , emailAddress: "Email address"
  , home: "Home"
  , password: "Password"
  , submit: "Submit"
  , userName: "User name"
  , userData: "User data"
  , profile: "Profile"
  , role: "Role"
  }

deCommon :: Translation CommonLabels
deCommon = fromRecord
  { email: "E-Mail"
  , emailAddress: "E-Mail-Adresse"
  , home: "Start"
  , password: "Passwort"
  , submit: "Absenden"
  , userName: "Benutzername"
  , userData: "Nutzerdaten"
  , profile: "Profil"
  , role: "Rolle"
  }

