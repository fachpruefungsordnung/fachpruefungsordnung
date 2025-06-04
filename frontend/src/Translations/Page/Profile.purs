module Translations.Profile where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type ProfileLabels =
  ( "prof_loginSuccessful"
      ::: "prof_profile"
      ::: "prof_role"
      ::: "prof_userData"
      ::: "prof_userName"
      ::: SNil
  )

enProfile :: Translation ProfileLabels
enProfile = fromRecord
  { prof_loginSuccessful: "Login successful"
  , prof_role: "Role"
  , prof_profile: "Profile"
  , prof_userName: "User name"
  , prof_userData: "User data"
  }

deProfile :: Translation ProfileLabels
deProfile = fromRecord
  { prof_loginSuccessful: "Login erfolgreich"
  , prof_role: "Rolle"
  , prof_profile: "Profil"
  , prof_userName: "Benutzername"
  , prof_userData: "Nutzerdaten"
  }
