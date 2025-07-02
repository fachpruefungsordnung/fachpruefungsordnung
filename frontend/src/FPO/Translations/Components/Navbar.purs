module FPO.Translations.Components.Navbar where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type NavbarLabels =
  ( "navbar_documents"
      ::: "navbar_groups"
      ::: "navbar_users"
      ::: SNil
  )

enNavbar :: Translation NavbarLabels
enNavbar = fromRecord
  { navbar_documents: "Documents"
  , navbar_groups: "Groups"
  , navbar_users: "Users"
  }

deNavbar :: Translation NavbarLabels
deNavbar = fromRecord
  { navbar_documents: "Dokumente"
  , navbar_groups: "Gruppen"
  , navbar_users: "Benutzer"
  }
