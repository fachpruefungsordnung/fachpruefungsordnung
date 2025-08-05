module FPO.Translations.Page.Admin.AddMembers where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AddMembersPageLabels =
  ( "gmam_addMember"
      ::: "gmam_assignMembers"
      ::: "gmam_failedToAdd"
      ::: "gmam_failedToRemove"
      ::: "gmam_groupNotFound"
      ::: "gmam_loadingGroup"
      ::: "gmam_removeMember"
      ::: SNil
  )

enAddMembersPage :: Translation AddMembersPageLabels
enAddMembersPage = fromRecord
  { gmam_addMember: "Add Member to Group"
  , gmam_assignMembers: "Assign Members to"
  , gmam_failedToAdd: "Failed to add user"
  , gmam_failedToRemove: "Failed to remove user"
  , gmam_groupNotFound: "Failed to retrieve group info"
  , gmam_loadingGroup: "Loading group information..."
  , gmam_removeMember: "Remove Member from Group"
  }

deAddMembersPage :: Translation AddMembersPageLabels
deAddMembersPage = fromRecord
  { gmam_addMember: "Mitglied zur Gruppe hinzufügen"
  , gmam_assignMembers: "Mitgliederzuweisung für"
  , gmam_failedToAdd: "Fehler beim Hinzufügen des Benutzers"
  , gmam_failedToRemove: "Fehler beim Entfernen des Benutzers"
  , gmam_groupNotFound: "Fehler beim Abrufen der Gruppeninformationen"
  , gmam_loadingGroup: "Lade Gruppeninformationen..."
  , gmam_removeMember: "Mitglied aus Gruppe entfernen"
  }
