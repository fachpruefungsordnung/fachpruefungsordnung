module FPO.Translations.Components.Editor
  ( deEditor
  , enEditor
  ) where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type EditorLabels =
  ( "editor_allComments"
      ::: "editor_comment"
      ::: "editor_deleteComment"
      ::: "editor_fontSizeDown"
      ::: "editor_fontSizeUp"
      ::: "editor_pdf"
      ::: "editor_preview"
      ::: "editor_redo"
      ::: "editor_save"
      ::: "editor_textBold"
      ::: "editor_textItalic"
      ::: "editor_textUnderline"
      ::: "editor_undo"
      ::: SNil
  )

enEditor :: Translation EditorLabels
enEditor = fromRecord
  { editor_allComments: "All comments"
  , editor_comment: "Comment"
  , editor_deleteComment: "Delete comment"
  , editor_fontSizeDown: "Font size down"
  , editor_fontSizeUp: "Font size up"
  , editor_pdf: "Export to PDF"
  , editor_preview: "Preview"
  , editor_redo: "Redo (Ctrl+Shift+Z)"
  , editor_save: "Save"
  , editor_textBold: "Bold text (Ctrl+B)"
  , editor_textItalic: "Italic text (Ctrl+I)"
  , editor_textUnderline: "Underline text"
  , editor_undo: "Undo (Ctrl+Z)"
  }

deEditor :: Translation EditorLabels
deEditor = fromRecord
  { editor_allComments: "Alle Kommentare"
  , editor_comment: "Kommentar"
  , editor_deleteComment: "Kommentar löschen"
  , editor_fontSizeDown: "Schrift verkleinern"
  , editor_fontSizeUp: "Schrift vergrößern"
  , editor_pdf: "Als PDF exportieren"
  , editor_preview: "Vorschau"
  , editor_redo: "Vor (Strg+Umschalt+Z)"
  , editor_save: "Speichern"
  , editor_textBold: "Text fett (Strg+B)"
  , editor_textItalic: "Text kursiv (Strg+I)"
  , editor_textUnderline: "Text unterstreichen"
  , editor_undo: "Zurück (Strg+Z)"
  }
