module Translations.Auth where

import Prelude

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AuthLabels =
  ( "passwordForgotten"
      ::: SNil
  )

enAuth :: Translation AuthLabels
enAuth = fromRecord
  { passwordForgotten: "Forgot password?"
  }

deAuth :: Translation AuthLabels
deAuth = fromRecord
  { passwordForgotten: "Passwort vergessen?"
  }
