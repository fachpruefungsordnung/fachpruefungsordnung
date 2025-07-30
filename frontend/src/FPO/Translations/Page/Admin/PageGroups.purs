module FPO.Translations.Page.Admin.PageGroups where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AdminGroupPageLabels =
  ( "admin_groups_createGroup"
      ::: "admin_groups_createNewGroup"
      ::: "admin_groups_desc"
      ::: "admin_groups_enterGroupDesc"
      ::: "admin_groups_enterGroupName"
      ::: "admin_groups_errCreatingGroup"
      ::: "admin_groups_errDecodingGroupId"
      ::: "admin_groups_errDeletingGroup"
      ::: "admin_groups_errNotFound"
      ::: "admin_groups_failedDeletingGroup"
      ::: "admin_groups_groupName"
      ::: "admin_groups_listOfGroups"
      ::: "admin_groups_notEmpty"
      ::: "admin_groups_searchForGroups"
      ::: "admin_groups_stillLoading"
      ::: "admin_groups_viewDocumentsPage"
      ::: SNil
  )

enAdminGroupPage :: Translation AdminGroupPageLabels
enAdminGroupPage = fromRecord
  { admin_groups_createGroup: "Create Group"
  , admin_groups_createNewGroup: "Create New Group"
  , admin_groups_desc: "Group description"
  , admin_groups_enterGroupDesc: "Enter group description (optional)"
  , admin_groups_enterGroupName: "Enter group name"
  , admin_groups_errCreatingGroup: "Error creating group"
  , admin_groups_errDecodingGroupId: "Error decoding group ID"
  , admin_groups_errDeletingGroup: "Error deleting group"
  , admin_groups_errNotFound: "Group not found."
  , admin_groups_failedDeletingGroup: "Failed to delete group."
  , admin_groups_groupName: "Group Name"
  , admin_groups_listOfGroups: "List of Groups"
  , admin_groups_notEmpty: "Group name cannot be empty."
  , admin_groups_searchForGroups: "Search for Groups"
  , admin_groups_stillLoading: "Groups are still loading."
  , admin_groups_viewDocumentsPage: "View Documents Page of Group"
  }

deAdminGroupPage :: Translation AdminGroupPageLabels
deAdminGroupPage = fromRecord
  { admin_groups_createGroup: "Gruppe erstellen"
  , admin_groups_createNewGroup: "Neue Gruppe erstellen"
  , admin_groups_desc: "Gruppenbeschreibung"
  , admin_groups_enterGroupDesc: "Gruppenbeschreibung eingeben (optional)"
  , admin_groups_enterGroupName: "Gruppennamen eingeben"
  , admin_groups_errCreatingGroup: "Fehler beim Erstellen der Gruppe"
  , admin_groups_errDecodingGroupId: "Fehler beim Dekodieren der Gruppen-ID"
  , admin_groups_errDeletingGroup: "Fehler beim Löschen der Gruppe"
  , admin_groups_errNotFound: "Gruppe nicht gefunden."
  , admin_groups_failedDeletingGroup: "Löschen der Gruppe fehlgeschlagen."
  , admin_groups_groupName: "Gruppenname"
  , admin_groups_listOfGroups: "Liste der Gruppen"
  , admin_groups_notEmpty: "Der Gruppenname darf nicht leer sein."
  , admin_groups_searchForGroups: "Nach Gruppen suchen"
  , admin_groups_stillLoading: "Gruppen werden noch geladen."
  , admin_groups_viewDocumentsPage: "Dokumente der Gruppe anzeigen"
  }

