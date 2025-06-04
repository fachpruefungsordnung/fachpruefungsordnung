module Translations.Home where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type HomeLabels =
  ( "home_pleaseLogIn"
      ::: "home_toLogin"
      ::: "home_yourProjects"
      ::: SNil
  )

enHome :: Translation HomeLabels
enHome = fromRecord
  { home_pleaseLogIn: "Please log in to view your projects."
  , home_toLogin: "To login"
  , home_yourProjects: "Your Projects"
  }

deHome :: Translation HomeLabels
deHome = fromRecord
  { home_pleaseLogIn: "Bitte logge Dich ein, damit Du deine Projekte sehen kannst."
  , home_toLogin: "Zum Login"
  , home_yourProjects: "Deine Projekte"
  }
