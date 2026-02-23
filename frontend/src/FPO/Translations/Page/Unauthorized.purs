module FPO.Translations.Page.Unauthorized where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type UnauthorizedLabels =
  ( "unauthorized_message"
      ::: "unauthorized_title"
      ::: SNil
  )

enUnauthorized :: Translation UnauthorizedLabels
enUnauthorized = fromRecord
  { unauthorized_message:
      "You do not have permission to view this page. Please contact an administrator if you believe this is an error."
  , unauthorized_title:
      "Access Denied"
  }

deUnauthorized :: Translation UnauthorizedLabels
deUnauthorized = fromRecord
  { unauthorized_message:
      "Sie haben keine Berechtigung, diese Seite anzuzeigen. Bitte wenden Sie sich an einen Administrator, falls Sie glauben, dass dies ein Fehler ist."
  , unauthorized_title:
      "Zugriff verweigert"
  }
