module FPO.Translations.Page.Admin.GroupProjects where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type GroupProjectsPageLabels =
  ( "gp_createNewProject"
      ::: "gp_documentName"
      ::: "gp_enterDocumentName"
      ::: "gp_groupProjects"
      ::: "gp_newProject"
      ::: "gp_projectManagement"
      ::: "gp_removeProject"
      ::: "gp_searchProjects"
      ::: SNil
  )

enGroupProjectsPage :: Translation GroupProjectsPageLabels
enGroupProjectsPage = fromRecord
  { gp_createNewProject: "Create New Project"
  , gp_documentName: "Document Name"
  , gp_enterDocumentName: "Enter Document Name"
  , gp_groupProjects: "Projects of Group"
  , gp_newProject: "Create Project"
  , gp_projectManagement: "Project Management"
  , gp_removeProject: "Remove Project"
  , gp_searchProjects: "Search for Projects"
  }

deGroupProjectsPage :: Translation GroupProjectsPageLabels
deGroupProjectsPage = fromRecord
  { gp_createNewProject: "Neues Projekt erstellen"
  , gp_documentName: "Dokumentname"
  , gp_enterDocumentName: "Dokumentname eingeben"
  , gp_groupProjects: "Projekte der Gruppe"
  , gp_newProject: "Neues Projekt"
  , gp_projectManagement: "Projektverwaltung"
  , gp_removeProject: "Projekt entfernen"
  , gp_searchProjects: "Suche nach Projekten"
  }
