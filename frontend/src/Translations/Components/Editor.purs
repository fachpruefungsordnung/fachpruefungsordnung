module Translations.Components.Editor where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type EditorLabels =
  ( "editor_comment"
      ::: "editor_deleteComment"
      ::: "editor_fontSizeDown"
      ::: "editor_fontSizeUp"
      ::: "editor_redo"
      ::: "editor_textBold"
      ::: "editor_textItalic"
      ::: "editor_textUnderline"
      ::: "editor_undo"
      ::: SNil
  )

enEditor :: Translation EditorLabels
enEditor = fromRecord
  { editor_comment: "Comment"
  , editor_deleteComment: "Delete comment"
  , editor_fontSizeDown: "Font size up"
  , editor_fontSizeUp: "Font size down"
  , editor_redo: "Redo (Ctrl+Shift+Z)"
  , editor_textBold: "Bold text (Ctrl+B)"
  , editor_textItalic: "Italic text (Ctrl+I)"
  , editor_textUnderline: "Underline text"
  , editor_undo: "Undo (Ctrl+Z)"
  }

deEditor :: Translation EditorLabels
deEditor = fromRecord
  { editor_comment: "Kommentar"
  , editor_deleteComment: "Kommentar löschen"
  , editor_fontSizeDown: "Schrift vergößern"
  , editor_fontSizeUp: "Schrift verkleinern"
  , editor_redo: "Vor (Strg+Umschalt+Z)"
  , editor_textBold: "Text fett (Strg+B)"
  , editor_textItalic: "Text kursiv (Strg+I)"
  , editor_textUnderline: "Text unterstreichen"
  , editor_undo: "Zurück (Strg+Z)"
  }
