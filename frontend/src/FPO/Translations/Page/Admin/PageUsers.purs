module FPO.Translations.Page.Admin.PageUsers where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AdminUserPageLabels =
  ( "admin_users_create"
      ::: "admin_users_createNewUser"
      ::: "admin_users_failedToCreateUser"
      ::: "admin_users_failedToDeleteUser"
      ::: "admin_users_failedToLoadUsers"
      ::: "admin_users_goToProfilePage"
      ::: "admin_users_listOfUsers"
      ::: "admin_users_successfullyCreatedUser"
      ::: "admin_users_theUser"
      ::: SNil
  )

enAdminUserPage :: Translation AdminUserPageLabels
enAdminUserPage = fromRecord
  { admin_users_listOfUsers: "List of Users"
  , admin_users_createNewUser: "Create New User"
  , admin_users_failedToCreateUser: "Failed to create user"
  , admin_users_failedToDeleteUser: "Failed to delete user"
  , admin_users_failedToLoadUsers: "Failed to load users"
  , admin_users_goToProfilePage: "Go to profile page"
  , admin_users_successfullyCreatedUser: "Successfully created user"
  , admin_users_create: "Create"
  , admin_users_theUser: "user"
  }

deAdminUserPage :: Translation AdminUserPageLabels
deAdminUserPage = fromRecord
  { admin_users_listOfUsers: "Liste der Nutzer"
  , admin_users_createNewUser: "Neuen Nutzer erstellen"
  , admin_users_failedToCreateUser: "Fehler beim Erstellen des Nutzers"
  , admin_users_failedToDeleteUser: "Fehler beim Löschen des Nutzers"
  , admin_users_failedToLoadUsers: "Fehler beim Laden der Nutzer"
  , admin_users_goToProfilePage: "Zum Profil gehen"
  , admin_users_successfullyCreatedUser: "Nutzer erfolgreich erstellt"
  , admin_users_create: "Erstellen"
  , admin_users_theUser: "den Nutzer"
  }

