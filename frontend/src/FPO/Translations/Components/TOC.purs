module FPO.Translations.Components.TOC where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type TocLabels =
  ( "toc_end_dropzone"
      ::: "toc_full"
      ::: "toc_paragraph"
      ::: "toc_section"
      ::: SNil
  )

enTOC :: Translation TocLabels
enTOC = fromRecord
  { toc_end_dropzone: "Drop here to add to end of section"
  , toc_full:
      "You've reached the end of the loaded versions. To see other versions, please search in a different timeframe"
  , toc_paragraph: "Paragraph"
  , toc_section: "Section"
  }

deTOC :: Translation TocLabels
deTOC = fromRecord
  { toc_end_dropzone: "Am Ende einfügen"
  , toc_full:
      "Sie haben das Ende der geladenen Versionen erreicht. Ändern sie bitte den Suchzeitraum, falls sie nach anderen Versionen suchen."
  , toc_paragraph: "den Paragraphen"
  , toc_section: "den Abschnitt"
  }
