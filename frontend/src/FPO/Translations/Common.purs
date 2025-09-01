module FPO.Translations.Common where

import Prelude ((<>))
import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type CommonLabels =
  ( "common_cancel"
      ::: "common_confirmDelete"
      ::: "common_create"
      ::: "common_delete"
      ::: "common_deletePhraseA"
      ::: "common_deletePhraseB"
      ::: "common_email"
      ::: "common_emailAddress"
      ::: "common_filterBy"
      ::: "common_group"
      ::: "common_home"
      ::: "common_member"
      ::: "common_members"
      ::: "common_membersOf"
      ::: "common_mergingInfo"
      ::: "common_mergingInfoText"
      ::: "common_password"
      ::: "common_passwordUpdated"
      ::: "common_project"
      ::: "common_projects"
      ::: "common_save"
      ::: "common_saving"
      ::: "common_sentResetLinkDone"
      ::: "common_submit"
      ::: "common_theGroup"
      ::: "common_user"
      ::: "common_userName"
      ::: SNil
  )

enCommon :: Translation CommonLabels
enCommon = fromRecord
  { common_cancel: "Cancel"
  , common_confirmDelete: "Confirm Delete"
  , common_create: "Create"
  , common_delete: "Delete"
  , common_deletePhraseA: "Are you sure you want to delete "
  , common_deletePhraseB: "?"
  , common_email: "Email"
  , common_emailAddress: "Email address"
  , common_filterBy: "Filter by"
  , common_group: "group"
  , common_home: "Home"
  , common_member: "Member"
  , common_members: "Members"
  , common_membersOf: "Members of "
  , common_mergingInfo: "Working on Outdated Files"
  , common_mergingInfoText: "You are currently editing an outdated version. \n\n"
      <> "How was this caused? \n"
      <> "As this website supports editing by multiple people simultaneously, "
      <> "it is possible that someone else started editing the same part of this "
      <>
        "file as you and saved their version. Alternatively, You may have simply openend "
      <> "an old version.\n\n"
      <> "What does this mean for you?\n"
      <>
        "You can keep editing this text element as usual, but once you are done and want to save your changes, you will need to "
      <>
        "merge your changes with the current version so you can ensure that no changes get lost. "
      <>
        "Otherwise, either your changes or the changes of the current file would get lost.\n"
      <>
        "Once you choose to save, the current version will be opened on the left side of your screen so you can compare the changes made "
      <>
        "and copy over any changes you want to keep. Once you are done, you can save again, at which point you will be up to date again.\n\n"
      <>
        "If you do not wish to save your currently opened version, you can click on \"discard\" to discard them and open the current version."
  , common_password: "Password"
  , common_passwordUpdated: "Password updated"
  , common_project: "project"
  , common_projects: "Projects"
  , common_save: "Save"
  , common_saving: "Saving..."
  , common_sentResetLinkDone: "Password reset link sent"
  , common_user: "User"
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
  , common_confirmDelete: "Löschen bestätigen"
  , common_create: "Erstellen"
  , common_delete: "Löschen"
  , common_deletePhraseA: "Sind Sie sicher, dass Sie "
  , common_deletePhraseB: " löschen möchten?"
  , common_email: "E-Mail"
  , common_emailAddress: "E-Mail-Addresse"
  , common_filterBy: "Filtern nach"
  , common_group: "Gruppe"
  , common_home: "Start"
  , common_member: "Mitglied"
  , common_members: "Mitglieder"
  , common_membersOf: "Mitglieder von "
  , common_mergingInfo: "Veraltete Versionen bearbeiten"
  , common_mergingInfoText: "You are currently editing an outdated version. \n\n"
      <> "How was this caused? \n"
      <> "As this website supports editing by multiple people simultaneously, "
      <> "it is possible that someone else started editing the same part of this "
      <>
        "file as you and saved their version. Alternatively, You may have simply openend "
      <> "an old version.\n\n"
      <> "What does this mean for you?\n"
      <>
        "You can keep editing this text element as usual, but once you are done and want to save your changes, you will need to "
      <>
        "merge your changes with the current version so you can ensure that no changes get lost. "
      <>
        "Otherwise, either your changes or the changes of the current file would get lost.\n"
      <>
        "Once you choose to save, the current version will be opened on the left side of your screen so you can compare the changes made "
      <>
        "and copy over any changes you want to keep. Once you are done, you can save again, at which point you will be up to date again.\n\n"
      <>
        "If you do not wish to save your currently opened version, you can click on \"discard\" to discard them and open the current version."
  , common_password: "Passwort"
  , common_passwordUpdated: "Passwort aktualisiert"
  , common_project: "Projekt"
  , common_projects: "Projekte"
  , common_save: "Speichern"
  , common_saving: "Speichere..."
  , common_sentResetLinkDone: "Link zum Zurücksetzen des Passworts gesendet"
  , common_user: "Benutzer"
  , common_userName: "Benutzername"
  , common_theGroup: "die Gruppe"
  , common_submit: "Absenden"
  }

