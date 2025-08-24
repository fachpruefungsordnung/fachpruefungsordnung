module FPO.Translations.Page.Home where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type HomeLabels =
  ( "home_basicDescription"
      ::: "home_editing"
      ::: "home_editingDescription"
      ::: "home_getStarted"
      ::: "home_learnMore"
      ::: "home_pleaseLogInA"
      ::: "home_pleaseLogInB"
      ::: "home_teamCollaboration"
      ::: "home_teamCollaborationDescription"
      ::: "home_toLogin"
      ::: "home_versionControl"
      ::: "home_versionControlDescription"
      ::: "home_yourProjects"
      ::: SNil
  )

enHome :: Translation HomeLabels
enHome = fromRecord
  { home_basicDescription:
      "Collaborative platform for creating and managing Fachprüfungsordnungen with version control and advanced editing tools."
  , home_editing: "Advanced Editing"
  , home_editingDescription:
      "Write using a dedicated markup language with live preview."
  , home_getStarted: "Get Started"
  , home_learnMore: "Learn More"
  , home_pleaseLogInA: "Please "
  , home_pleaseLogInB: " to see your projects."
  , home_teamCollaboration: "Team Collaboration"
  , home_teamCollaborationDescription:
      "Create projects, manage groups, and collaborate with university personnel."
  , home_toLogin: "log in"
  , home_versionControl: "Version Control"
  , home_versionControlDescription:
      "Track changes and maintain document history for all regulations."
  , home_yourProjects: "Your Projects"
  }

deHome :: Translation HomeLabels
deHome = fromRecord
  { home_basicDescription:
      "Kollaborative Plattform zur Erstellung und Verwaltung von Fachprüfungsordnungen mit Versionskontrolle und erweiterten Bearbeitungswerkzeugen."
  , home_editing: "Fortgeschrittene Bearbeitung"
  , home_editingDescription:
      "Schreibe mit einer dedizierten Auszeichnungssprache und Live-Vorschau."
  , home_getStarted: "Loslegen"
  , home_learnMore: "Mehr erfahren"
  , home_teamCollaboration: "Teamarbeit"
  , home_teamCollaborationDescription:
      "Erstelle Projekte, verwalte Gruppen und arbeite mit Universitätsmitarbeitern zusammen."
  , home_versionControl: "Versionskontrolle"
  , home_versionControlDescription:
      "Verfolge Änderungen und pflege die Dokumentenhistorie für alle Ordnungen."
  , home_pleaseLogInA: "Bitte "
  , home_pleaseLogInB: ", damit Du deine Projekte sehen kannst."
  , home_toLogin: "logge Dich ein"
  , home_yourProjects: "Deine Projekte"
  }
