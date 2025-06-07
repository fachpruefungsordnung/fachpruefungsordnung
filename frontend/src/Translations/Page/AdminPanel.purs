module FPO.Translations.AdminPanel where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type AdminPanelLabels =
  ( "ap_adminPanel"
      ::: SNil
  )

enAdminPanel :: Translation AdminPanelLabels
enAdminPanel = fromRecord
  { ap_adminPanel: "Admin Panel"
  }

deAdminPanel :: Translation AdminPanelLabels
deAdminPanel = fromRecord
  { ap_adminPanel: "Adminpanel"
  }

