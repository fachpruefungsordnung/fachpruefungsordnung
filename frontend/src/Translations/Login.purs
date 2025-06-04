module Translations.Login where

import Prelude

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord, toRecord)

type LoginLabels =
  ( "login_email"
      ::: "login_emailAddress"
      ::: "login_password"
      ::: "login_passwordForgotten"
      ::: SNil
  )

enLogin :: Translation LoginLabels
enLogin = fromRecord
  { login_email: "Email"
  , login_emailAddress: "Email address"
  , login_password: "Password"
  , login_passwordForgotten: "Forgot password?"
  }

deLogin :: Translation LoginLabels
deLogin = fromRecord
  { login_email: "E-Mail"
  , login_emailAddress: "E-Mail Addresse"
  , login_password: "Passwort"
  , login_passwordForgotten: "Passwort vergessen?"
  }
