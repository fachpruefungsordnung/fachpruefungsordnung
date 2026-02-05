module FPO.Translations.Page.AdminPanel
  ( AdminPanelLabels
  , deAdminPanel
  , enAdminPanel
  ) where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AdminPanelLabels =
  ( "admin_administration"
      ::: "admin_groups"
      ::: "admin_users"
      ::: "admin_users_searchUsers"
      ::: "au_groupManagement"
      ::: "au_userManagement"
      ::: SNil
  )

enAdminPanel :: Translation AdminPanelLabels
enAdminPanel = fromRecord
  { admin_administration: "Administration"
  , admin_groups: "Groups"
  , admin_users: "Users"
  , admin_users_searchUsers: "Search users..."
  , au_groupManagement: "Group Management"
  , au_userManagement: "User Management"
  }

deAdminPanel :: Translation AdminPanelLabels
deAdminPanel = fromRecord
  { admin_administration: "Verwaltung"
  , admin_groups: "Gruppen"
  , admin_users: "Benutzer"
  , admin_users_searchUsers: "Benutzer suchen..."
  , au_groupManagement: "Gruppenverwaltung"
  , au_userManagement: "Nutzerverwaltung"
  }
