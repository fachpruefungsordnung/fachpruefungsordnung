module FPO.Translations.Page.Admin.GroupMembers where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type GroupMemberPageLabels =
  ( "gm_addMember"
      ::: "gm_memberManagement"
      ::: "gm_membersAddedSuccessfully"
      ::: "gm_membersOfGroup"
      ::: "gm_noUsersFound"
      ::: "gm_removeMember"
      ::: "gm_role"
      ::: "gm_searchMembers"
      ::: SNil
  )

enGroupMemberPage :: Translation GroupMemberPageLabels
enGroupMemberPage = fromRecord
  { gm_addMember: "Add Member"
  , gm_memberManagement: "Member Management"
  , gm_membersAddedSuccessfully: "Members added successfully"
  , gm_membersOfGroup: "Members of Group"
  , gm_noUsersFound: "No users found"
  , gm_removeMember: "Remove Member"
  , gm_role: "Role"
  , gm_searchMembers: "Search for members"
  }

deGroupMemberPage :: Translation GroupMemberPageLabels
deGroupMemberPage = fromRecord
  { gm_addMember: "Mitglied hinzufügen"
  , gm_memberManagement: "Mitgliederverwaltung"
  , gm_membersAddedSuccessfully: "Mitglieder erfolgreich hinzugefügt"
  , gm_membersOfGroup: "Mitglieder der Gruppe"
  , gm_noUsersFound: "Keine Benutzer gefunden"
  , gm_removeMember: "Mitglied entfernen"
  , gm_role: "Rolle"
  , gm_searchMembers: "Nach Mitgliedern suchen"
  }
