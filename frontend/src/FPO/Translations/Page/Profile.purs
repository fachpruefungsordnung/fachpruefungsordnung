module FPO.Translations.Page.Profile where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type ProfileLabels =
  ( "prof_loginSuccessful"
      ::: "prof_profile"
      ::: "prof_role"
      ::: "prof_userData"
      ::: SNil
  )

enProfile :: Translation ProfileLabels
enProfile = fromRecord
  { prof_loginSuccessful: "Login successful"
  , prof_role: "Role"
  , prof_profile: "Profile"
  , prof_userData: "User data"
  }

deProfile :: Translation ProfileLabels
deProfile = fromRecord
  { prof_loginSuccessful: "Login erfolgreich"
  , prof_role: "Rolle"
  , prof_profile: "Profil"
  , prof_userData: "Nutzerdaten"
  }
