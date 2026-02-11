module FPO.Translations.Page.Page404 where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type Page404Labels =
  ( "p404_notFound"
      ::: SNil
  )

enPage404 :: Translation Page404Labels
enPage404 = fromRecord
  { p404_notFound:
      "The page you are looking for does not exist or has been moved."
  }

dePage404 :: Translation Page404Labels
dePage404 = fromRecord
  { p404_notFound:
      "Die Seite, die Sie suchen, existiert nicht oder wurde verschoben."
  }
