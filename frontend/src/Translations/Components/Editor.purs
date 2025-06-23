module Translations.Components.Editor where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type EditorLabels =
  ( "editor_textBold"
      ::: "editor_textItalic"
      ::: "editor_textUnderline"
      ::: SNil
  )

enEditor :: Translation EditorLabels
enEditor = fromRecord
  { editor_textBold: "Bold text (Ctrl+B)"
  , editor_textItalic: "Italic text (Ctrl+I)"
  , editor_textUnderline: "Underline text"
  }

deEditor :: Translation EditorLabels
deEditor = fromRecord
  { editor_textBold: "Text fett (Strg+B)"
  , editor_textItalic: "Text kursiv (Strg+I)"
  , editor_textUnderline: "Text unterstreichen"
  }
