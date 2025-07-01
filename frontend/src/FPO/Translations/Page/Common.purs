module FPO.Translations.Page.Common where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type CommonLabels =
  ( "common_email"
      ::: "common_emailAddress"
      ::: "common_filterBy"
      ::: "common_home"
      ::: "common_password"
      ::: "common_submit"
      ::: "common_userName"
      ::: SNil
  )

enCommon :: Translation CommonLabels
enCommon = fromRecord
  { common_email: "Email"
  , common_emailAddress: "Email address"
  , common_filterBy: "Filter by"
  , common_home: "Home"
  , common_password: "Password"
  , common_userName: "User name"
  , common_submit: "Submit"
  }

deCommon :: Translation CommonLabels
deCommon = fromRecord
  { common_email: "E-Mail"
  , common_emailAddress: "E-Mail Addresse"
  , common_filterBy: "Filtern nach"
  , common_home: "Start"
  , common_password: "Passwort"
  , common_userName: "Benutzername"
  , common_submit: "Absenden"
  }

