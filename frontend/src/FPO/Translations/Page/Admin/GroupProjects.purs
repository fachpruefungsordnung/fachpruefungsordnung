module FPO.Translations.Page.Admin.GroupProjects where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type GroupProjectsPageLabels =
  ( "gp_groupProjects"
      ::: "gp_newProject"
      ::: "gp_projectManagement"
      ::: "gp_searchProjects"
      ::: SNil
  )

enGroupProjectsPage :: Translation GroupProjectsPageLabels
enGroupProjectsPage = fromRecord
  { gp_groupProjects: "Projects of Group"
  , gp_newProject: "Create Project"
  , gp_projectManagement: "Project Management"
  , gp_searchProjects: "Search for Projects"
  }

deGroupProjectsPage :: Translation GroupProjectsPageLabels
deGroupProjectsPage = fromRecord
  { gp_groupProjects: "Projekte der Gruppe"
  , gp_newProject: "Neues Projekt"
  , gp_projectManagement: "Projektverwaltung"
  , gp_searchProjects: "Suche nach Projekten"
  }