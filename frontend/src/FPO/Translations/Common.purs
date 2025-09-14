module FPO.Translations.Common where

import Prelude ((<>))
import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type CommonLabels =
  ( "common_cancel"
      ::: "common_confirmDelete"
      ::: "common_confirmDiscard"
      ::: "common_create"
      ::: "common_delete"
      ::: "common_deletePhraseA"
      ::: "common_deletePhraseB"
      ::: "common_discard"
      ::: "common_discardPhrase"
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
      ::: "common_resolve"
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
  , common_confirmDiscard: "Confirm Discard"
  , common_create: "Create"
  , common_delete: "Delete"
  , common_deletePhraseA: "Are you sure you want to delete "
  , common_deletePhraseB: "?"
  , common_discard: "Discard"
  , common_discardPhrase:
      "Are you sure you want to discard the current changes to this text element?"
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
        "If you do not wish to save your currently opened version, you can click on \"Discard\" to discard them and open the current version."
  , common_password: "Password"
  , common_passwordUpdated: "Password updated"
  , common_project: "project"
  , common_projects: "Projects"
  , common_resolve: "Resolve"
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
  , common_confirmDiscard: "Verwerfen bestätigen"
  , common_create: "Erstellen"
  , common_delete: "Löschen"
  , common_deletePhraseA: "Sind Sie sicher, dass Sie "
  , common_deletePhraseB: " löschen möchten?"
  , common_discard: "Verwerfen"
  , common_discardPhrase:
      "Sind sie sicher, dass sie die Änderungen an diesem Textelement verwerfen möchten?"
  , common_email: "E-Mail"
  , common_emailAddress: "E-Mail-Addresse"
  , common_filterBy: "Filtern nach"
  , common_group: "Gruppe"
  , common_home: "Start"
  , common_member: "Mitglied"
  , common_members: "Mitglieder"
  , common_membersOf: "Mitglieder von "
  , common_mergingInfo: "Veraltete Versionen bearbeiten"
  , common_mergingInfoText: "Sie bearbeiten aktuell eine veraltete Version. \n\n"
      <> "Woher kommt das? \n"
      <>
        "Da diese Webseite es erlaubt, mehreren Nutzer/-innen gleichzeitig ein Dokument bearbeiten zu lassen, "
      <>
        "ist es möglich, dass eine anderere Person gerade die gleiche Textstelle bearbeitet"
      <>
        "und ihre Version gespeichert hat. Möglicherweise haben sie auch einfach über die Textabschnittsauswahl links "
      <> "eine alte Version geöffnet.\n\n"
      <> "Was bedeutet das?\n"
      <>
        "Sie können diesen Textabschnitt weiterhin wie gewohnt bearbeiten, aber wenn sie fertig sind mit diesem Abschnitt müssen sie ihre Änderungen"
      <>
        "mit denen der aktuellen Version zusammenführen, damit keine änderungen verloren gehen. "
      <>
        "Andernfalls würden entweder ihre Änderungen oder jene der aktuellen Version verloren gehen.\n"
      <>
        "Sobald sie ihre Änderungen speichern wird sich rechts die aktuelle Version öffnen. Editieren Sie ihre Version auf der linken Seite dann"
      <>
        "so, dass alle gewünschten änderungen beider Version in ihr enthalten sind. Sobald sie damit fertig sind, können sie durch ein weiteres"
      <>
        "klicken auf \"speichern\" ihre Änderungen speichern\n\n"
      <>
        "Falls sie wünschen, ihre aktuellen änderungen zu verwerfen, so könne sie auf \"Verwerfen\" klicken um sie zu verwerfen und zur aktuellen Version zurückzukehren."
  , common_password: "Passwort"
  , common_passwordUpdated: "Passwort aktualisiert"
  , common_project: "Projekt"
  , common_projects: "Projekte"
  , common_resolve: "Auflösen"
  , common_save: "Speichern"
  , common_saving: "Speichere..."
  , common_sentResetLinkDone: "Link zum Zurücksetzen des Passworts gesendet"
  , common_user: "Benutzer"
  , common_userName: "Benutzername"
  , common_theGroup: "die Gruppe"
  , common_submit: "Absenden"
  }

