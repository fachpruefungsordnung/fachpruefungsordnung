module FPO.Translations.Common where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type CommonLabels =
  ( "common_cancel"
      ::: "common_create"
      ::: "common_delete"
      ::: "common_deletePhraseA"
      ::: "common_deletePhraseB"
      ::: "common_document"
      ::: "common_email"
      ::: "common_emailAddress"
      ::: "common_filterBy"
      ::: "common_group"
      ::: "common_home"
      ::: "common_password"
      ::: "common_submit"
      ::: "common_theGroup"
      ::: "common_userName"
      ::: SNil
  )

enCommon :: Translation CommonLabels
enCommon = fromRecord
  { common_cancel: "Cancel"
  , common_create: "Create"
  , common_delete: "Delete"
  , common_deletePhraseA: "Are you sure you want to delete "
  , common_deletePhraseB: "?"
  , common_document: "document"
  , common_email: "Email"
  , common_emailAddress: "Email address"
  , common_filterBy: "Filter by"
  , common_group: "group"
  , common_home: "Home"
  , common_password: "Password"
  , common_userName: "User name"
  -- TODO: Change to "the group"? Not sure if this sounds better
  --       in english, but using the article might sound better in
  --       german in certain phrase constructions.
  , common_theGroup: "group"
  , common_submit: "Submit"
  }

deCommon :: Translation CommonLabels
deCommon = fromRecord
  { common_cancel: "Abbrechen"
  , common_create: "Erstellen"
  , common_delete: "Löschen"
  , common_deletePhraseA: "Sind Sie sicher, dass Sie "
  , common_deletePhraseB: " löschen möchten?"
  , common_document: "Dokument"
  , common_email: "E-Mail"
  , common_emailAddress: "E-Mail Addresse"
  , common_filterBy: "Filtern nach"
  , common_group: "Gruppe"
  , common_home: "Start"
  , common_password: "Passwort"
  , common_userName: "Benutzername"
  , common_theGroup: "die Gruppe"
  , common_submit: "Absenden"
  }

