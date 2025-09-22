module FPO.Translations.Components.Editor
  ( deEditor
  , enEditor
  ) where

import Prelude ((<>))
import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type EditorLabels =
  ( "editor_allComments"
      ::: "editor_changeVersion"
      ::: "editor_comment"
      ::: "editor_compareVersion"
      ::: "editor_confirmSwitch"
      ::: "editor_deleteComment"
      ::: "editor_dirtySwitch"
      ::: "editor_discard"
      ::: "editor_fontSizeDown"
      ::: "editor_fontSizeUp"
      ::: "editor_merge"
      ::: "editor_mergingInfo"
      ::: "editor_mergingInfoText"
      ::: "editor_mergingNow"
      ::: "editor_no_title"
      ::: "editor_oldVersion"
      ::: "editor_pdf"
      ::: "editor_preview"
      ::: "editor_readonly"
      ::: "editor_redo"
      ::: "editor_save"
      ::: "editor_textBold"
      ::: "editor_textItalic"
      ::: "editor_textUnderline"
      ::: "editor_undo"
      ::: "editor_viewVersion"
      ::: SNil
  )

enEditor :: Translation EditorLabels
enEditor = fromRecord
  { editor_allComments: "All comments"
  , editor_changeVersion: "Change Version"
  , editor_confirmSwitch: "Switching Version"
  , editor_comment: "Comment"
  , editor_compareVersion: "Compare Version"
  , editor_deleteComment: "Delete comment"
  , editor_dirtySwitch:
      "You currently have unsaved changes made to the current version. Switching between versions now will discard these changes."
  , editor_discard: "discard"
  , editor_fontSizeDown: "Font size down"
  , editor_fontSizeUp: "Font size up"
  , editor_merge: "Merge"
  , editor_mergingInfo: "Working on Outdated Files"
  , editor_mergingInfoText: "You are currently editing an outdated version. \n\n"
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
  , editor_mergingNow:
      "Copy over desired changes from the right and finish by clicking on \"Merge\""
  , editor_no_title: "No section selected."
  , editor_oldVersion: "You are editing an old version"
  , editor_pdf: "Export PDF"
  , editor_preview: "Preview"
  , editor_readonly:
      "This view is readonly. In order to edit it's content, please open it on the left side"
  , editor_redo: "Redo (Ctrl+Shift+Z)"
  , editor_save: "Save"
  , editor_textBold: "Bold text (Ctrl+B)"
  , editor_textItalic: "Italic text (Ctrl+I)"
  , editor_textUnderline: "Underline text"
  , editor_undo: "Undo (Ctrl+Z)"
  , editor_viewVersion: "View Version"
  }

deEditor :: Translation EditorLabels
deEditor = fromRecord
  { editor_allComments: "Alle Kommentare"
  , editor_changeVersion: "Version Wechseln"
  , editor_confirmSwitch: "Version Wechseln"
  , editor_comment: "Kommentar"
  , editor_compareVersion: "Version vergleichen"
  , editor_deleteComment: "Kommentar löschen"
  , editor_dirtySwitch:
      "Der aktuelle Abschnitt beinhält ungespeicherte Änderungen. Diese werden beim Wechseln der Version verworfen."
  , editor_discard: "verwerfen"
  , editor_fontSizeDown: "Schrift verkleinern"
  , editor_fontSizeUp: "Schrift vergrößern"
  , editor_merge: "Vereinen"
  , editor_mergingInfo: "Veraltete Versionen bearbeiten"
  , editor_mergingInfoText: "Sie bearbeiten aktuell eine veraltete Version. \n\n"
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
  , editor_mergingNow:
      "Bitte übernehmen sie gewünschten Änderungen der rechten Seite und klicken sie danach auf \"Vereinen\""
  , editor_no_title: "Kein Abschnitt ausgewählt."
  , editor_oldVersion: "Sie bearbeiten eine alte Version"
  , editor_pdf: "PDF exportieren"
  , editor_preview: "Vorschau"
  , editor_readonly:
      "Diese Ansicht ist schreibgeschützt. Der Text muss zum bearbeiten auf der linke Seite geöffnet sein."
  , editor_redo: "Vor (Strg+Umschalt+Z)"
  , editor_save: "Speichern"
  , editor_textBold: "Text fett (Strg+B)"
  , editor_textItalic: "Text kursiv (Strg+I)"
  , editor_textUnderline: "Text unterstreichen"
  , editor_undo: "Zurück (Strg+Z)"
  , editor_viewVersion: "Zu Version Wechseln"
  }
