module FPO.Translations.Page.Admin.GroupSettings where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type GroupSettingsPageLabels =
  ( "gs_description"
      ::: "gs_descriptionPlaceholder"
      ::: "gs_groupName"
      ::: "gs_groupNamePlaceholder"
      ::: "gs_saveSettings"
      ::: "gs_settings"
      ::: "gs_settingsUpdated"
      ::: SNil
  )

enGroupSettingsPage :: Translation GroupSettingsPageLabels
enGroupSettingsPage = fromRecord
  { gs_description: "Description"
  , gs_descriptionPlaceholder: "Enter group description (optional)"
  , gs_groupName: "Group Name"
  , gs_groupNamePlaceholder: "Enter group name"
  , gs_saveSettings: "Save Settings"
  , gs_settings: "Settings"
  , gs_settingsUpdated: "Group settings updated successfully"
  }

deGroupSettingsPage :: Translation GroupSettingsPageLabels
deGroupSettingsPage = fromRecord
  { gs_description: "Beschreibung"
  , gs_descriptionPlaceholder: "Gruppenbeschreibung eingeben (optional)"
  , gs_groupName: "Gruppenname"
  , gs_groupNamePlaceholder: "Gruppenname eingeben"
  , gs_saveSettings: "Einstellungen speichern"
  , gs_settings: "Einstellungen"
  , gs_settingsUpdated: "Gruppeneinstellungen erfolgreich aktualisiert"
  }
