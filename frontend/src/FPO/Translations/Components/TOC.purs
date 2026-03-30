module FPO.Translations.Components.TOC where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type TocLabels =
  ( "toc_cannotDelete"
      ::: "toc_editModal_hint"
      ::: "toc_editModal_title"
      ::: "toc_editMode_off"
      ::: "toc_editMode_on"
      ::: "toc_end_dropzone"
      ::: "toc_full"
      ::: "toc_paragraph"
      ::: "toc_section"
      ::: SNil
  )

enTOC :: Translation TocLabels
enTOC = fromRecord
  { toc_cannotDelete: "This element cannot be removed"
  , toc_editModal_hint: "Drag entries to reorder. Use + to add and \x2212 to remove elements."
  , toc_editModal_title: "Edit Document Structure"
  , toc_editMode_off: "Stop editing"
  , toc_editMode_on: "Edit structure"
  , toc_end_dropzone: "Drop here to add to end of section"
  , toc_full:
      "You've reached the end of the loaded versions. To see other versions, please search in a different timeframe"
  , toc_paragraph: "Paragraph"
  , toc_section: "Section"
  }

deTOC :: Translation TocLabels
deTOC = fromRecord
  { toc_cannotDelete: "Dieses Element kann nicht entfernt werden"
  , toc_editModal_hint: "Eintr\x00E4ge ziehen zum Neuordnen. Mit + hinzuf\x00FCgen und \x2212 entfernen."
  , toc_editModal_title: "Dokumentstruktur bearbeiten"
  , toc_editMode_off: "Bearbeitung beenden"
  , toc_editMode_on: "Struktur bearbeiten"
  , toc_end_dropzone: "Am Ende einfügen"
  , toc_full:
      "Sie haben das Ende der geladenen Versionen erreicht. Ändern sie bitte den Suchzeitraum, falls sie nach anderen Versionen suchen."
  , toc_paragraph: "den Paragraphen"
  , toc_section: "den Abschnitt"
  }
