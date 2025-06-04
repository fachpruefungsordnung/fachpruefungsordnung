module Translations.Common where

import Prelude

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type CommonLabels =
  ( "email"
      ::: "emailAddress"
      ::: "home"
      ::: "password"
      ::: "submit"
      ::: SNil
  )

enCommon :: Translation CommonLabels
enCommon = fromRecord
  { email: "Email"
  , emailAddress: "Email address"
  , home: "Home"
  , password: "Password"
  , submit: "Submit"
  }

deCommon :: Translation CommonLabels
deCommon = fromRecord
  { email: "E-Mail"
  , emailAddress: "E-Mail-Adresse"
  , home: "Start"
  , password: "Passwort"
  , submit: "Absenden"
  }

