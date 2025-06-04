module Translations.Auth where

import Prelude

import Record.Extra (SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AuthLabels =
  ("loginSuccessful" ::: "passwordForgotten" ::: SNil)

enAuth :: Translation AuthLabels
enAuth = fromRecord
  { loginSuccessful: "Login successful"
  , passwordForgotten: "Forgot password?"
  }

deAuth :: Translation AuthLabels
deAuth = fromRecord
  { loginSuccessful: "Login erfolgreich"
  , passwordForgotten: "Passwort vergessen?"
  }
