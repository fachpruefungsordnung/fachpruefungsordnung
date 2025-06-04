module Translations.Common where

import Prelude

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type CommonLabels =
  ( "common_email"
      ::: "common_emailAddress"
      ::: "common_password"
      ::: "common_submit"
      ::: SNil
  )

enCommon :: Translation CommonLabels
enCommon = fromRecord
  { common_email: "Email"
  , common_emailAddress: "Email address"
  , common_password: "Password"
  , common_submit: "Submit"
  }

deCommon :: Translation CommonLabels
deCommon = fromRecord
  { common_email: "E-Mail"
  , common_emailAddress: "E-Mail Addresse"
  , common_password: "Passwort"
  , common_submit: "Absenden"
  }

