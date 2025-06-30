module FPO.Translations.AdminPanel where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AdminPanelLabels =
  ( "au_groupManagement"
      ::: "au_userManagement"
      ::: SNil
  )

enAdminPanel :: Translation AdminPanelLabels
enAdminPanel = fromRecord
  { au_groupManagement: "Group Management"
  , au_userManagement: "User Management"
  }

deAdminPanel :: Translation AdminPanelLabels
deAdminPanel = fromRecord
  { au_groupManagement: "Gruppenverwaltung"
  , au_userManagement: "Nutzerverwaltung"
  }

