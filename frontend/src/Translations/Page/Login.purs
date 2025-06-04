module Translations.Login where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type LoginLabels =
  ( "login_passwordForgotten"
      ::: SNil
  )

enLogin :: Translation LoginLabels
enLogin = fromRecord
  { login_passwordForgotten: "Forgot password?"
  }

deLogin :: Translation LoginLabels
deLogin = fromRecord
  { login_passwordForgotten: "Passwort vergessen?"
  }
