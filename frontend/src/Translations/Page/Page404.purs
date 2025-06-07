module Translations.Page.Page404 where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type Page404Labels =
  ( "p404_notFound"
      ::: SNil
  )

enPage404 :: Translation Page404Labels
enPage404 = fromRecord
  { p404_notFound:
      "This page does not exist, has been moved, or requires privileges you do not have."
  }

dePage404 :: Translation Page404Labels
dePage404 = fromRecord
  { p404_notFound:
      "Diese Seite existiert nicht, wurde verschoben oder erfordert Rechte, die Sie nicht haben."
  }
