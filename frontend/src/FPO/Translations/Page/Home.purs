module FPO.Translations.Page.Home where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type HomeLabels =
  ( "home_pleaseLogInA"
      ::: "home_pleaseLogInB"
      ::: "home_toLogin"
      ::: "home_yourProjects"
      ::: SNil
  )

enHome :: Translation HomeLabels
enHome = fromRecord
  { home_pleaseLogInA: "Please "
  , home_pleaseLogInB: " to see your projects."
  , home_toLogin: "log in"
  , home_yourProjects: "Your Projects"
  }

deHome :: Translation HomeLabels
deHome = fromRecord
  { home_pleaseLogInA: "Bitte "
  , home_pleaseLogInB: ", damit Du deine Projekte sehen kannst."
  , home_toLogin: "logge Dich ein"
  , home_yourProjects: "Deine Projekte"
  }
