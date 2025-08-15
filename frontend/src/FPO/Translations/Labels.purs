module FPO.Translations.Labels where

import Data.Function (($))
import FPO.Translations.Common (deCommon, enCommon)
import FPO.Translations.Components.Editor (deEditor, enEditor)
import FPO.Translations.Components.Navbar (deNavbar, enNavbar)
import FPO.Translations.Components.TOC (deTOC, enTOC)
import FPO.Translations.Page.Admin.AddMembers (deAddMembersPage, enAddMembersPage)
import FPO.Translations.Page.Admin.GroupMembers (deGroupMemberPage, enGroupMemberPage)
import FPO.Translations.Page.Admin.GroupProjects
  ( deGroupProjectsPage
  , enGroupProjectsPage
  )
import FPO.Translations.Page.Admin.PageGroups (deAdminGroupPage, enAdminGroupPage)
import FPO.Translations.Page.Admin.PageUsers (deAdminUserPage, enAdminUserPage)
import FPO.Translations.Page.AdminPanel (deAdminPanel, enAdminPanel)
import FPO.Translations.Page.Home (deHome, enHome)
import FPO.Translations.Page.Login (deLogin, enLogin)
import FPO.Translations.Page.Page404 (dePage404, enPage404)
import FPO.Translations.Page.Profile (deProfile, enProfile)
import FPO.Translations.Page.ResetPassword (dePasswordReset, enPasswordReset)
import Record (merge)
import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord, toRecord)

en :: Translation Labels
en = fromRecord
  $ merge (toRecord enTOC)
  $ merge (toRecord enAdminPanel)
  $ merge (toRecord enAdminGroupPage)
  $ merge (toRecord enAdminUserPage)
  $ merge (toRecord enAddMembersPage)
  $ merge (toRecord enCommon)
  $ merge (toRecord enEditor)
  $ merge (toRecord enNavbar)
  $ merge (toRecord enHome)
  $ merge (toRecord enLogin)
  $ merge (toRecord enPage404)
  $ merge (toRecord enPasswordReset)
  $ merge (toRecord enProfile)
  $ merge (toRecord enGroupProjectsPage)
  $
    toRecord enGroupMemberPage

de :: Translation Labels
de = fromRecord
  $ merge (toRecord deTOC)
  $ merge (toRecord deAdminPanel)
  $ merge (toRecord deAdminGroupPage)
  $ merge (toRecord deAdminUserPage)
  $ merge (toRecord deAddMembersPage)
  $ merge (toRecord deCommon)
  $ merge (toRecord deEditor)
  $ merge (toRecord deNavbar)
  $ merge (toRecord deHome)
  $ merge (toRecord deLogin)
  $ merge (toRecord dePage404)
  $ merge (toRecord dePasswordReset)
  $ merge (toRecord deProfile)
  $ merge (toRecord deGroupProjectsPage)
  $
    toRecord deGroupMemberPage

-- | All kinds of abstract labels representing UI texts,
-- | detached from the actual language selection.
-- |
-- | Symbols MUST be in alphabetic order.
-- | Because of this constraint, it's sensible to use
-- | appropriate prefixes for strongly related labels.
type Labels =
  ( -- | Admin Groups Page
    "admin_groups_createGroup"
      ::: "admin_groups_createNewGroup"
      ::: "admin_groups_desc"
      ::: "admin_groups_enterGroupDesc"
      ::: "admin_groups_enterGroupName"
      ::: "admin_groups_errCreatingGroup"
      ::: "admin_groups_errDecodingGroupId"
      ::: "admin_groups_errDeletingGroup"
      ::: "admin_groups_errNotFound"
      ::: "admin_groups_failedDeletingGroup"
      ::: "admin_groups_groupName"
      ::: "admin_groups_listOfGroups"
      ::: "admin_groups_notEmpty"
      ::: "admin_groups_searchForGroups"
      ::: "admin_groups_stillLoading"
      ::: "admin_groups_viewDocumentsPage"

      -- | Admin Users Page
      ::: "admin_users_create"
      ::: "admin_users_createNewUser"
      ::: "admin_users_deleteUser"
      ::: "admin_users_failedToCreateUser"
      ::: "admin_users_failedToDeleteUser"
      ::: "admin_users_failedToLoadUsers"
      ::: "admin_users_goToProfilePage"
      ::: "admin_users_listOfUsers"
      ::: "admin_users_successfullyCreatedUser"
      ::: "admin_users_theUser"

      -- | Admin Panel
      ::: "au_groupManagement"
      ::: "au_userManagement"

      -- | Common Phrases
      ::: "common_cancel"
      ::: "common_confirmDelete"
      ::: "common_create"
      ::: "common_delete"
      ::: "common_deletePhraseA"
      ::: "common_deletePhraseB"
      ::: "common_email"
      ::: "common_emailAddress"
      ::: "common_filterBy"
      ::: "common_group"
      ::: "common_home"
      ::: "common_member"
      ::: "common_members"
      ::: "common_membersOf"
      ::: "common_password"
      ::: "common_project"
      ::: "common_projects"
      ::: "common_save"
      ::: "common_saving"
      ::: "common_submit"
      ::: "common_theGroup"
      ::: "common_user"
      ::: "common_userName"

      -- | Editor Page
      ::: "editor_allComments"
      ::: "editor_comment"
      ::: "editor_deleteComment"
      ::: "editor_fontSizeDown"
      ::: "editor_fontSizeUp"
      ::: "editor_pdf"
      ::: "editor_preview"
      ::: "editor_redo"
      ::: "editor_save"
      ::: "editor_textBold"
      ::: "editor_textItalic"
      ::: "editor_textUnderline"
      ::: "editor_undo"

      -- | Group Members Page
      ::: "gm_addMember"
      ::: "gm_memberManagement"
      ::: "gm_membersOfGroup"
      ::: "gm_removeMember"
      ::: "gm_role"
      ::: "gm_searchMembers"

      -- | Group Manamgent - Add Members Page
      ::: "gmam_addMember"
      ::: "gmam_assignMembers"
      ::: "gmam_failedToAdd"
      ::: "gmam_failedToRemove"
      ::: "gmam_groupNotFound"
      ::: "gmam_loadingGroup"
      ::: "gmam_removeMember"

      -- | Group Projects Page
      ::: "gp_createNewProject"
      ::: "gp_documentName"
      ::: "gp_enterDocumentName"
      ::: "gp_groupProjects"
      ::: "gp_newProject"
      ::: "gp_projectManagement"
      ::: "gp_removeProject"
      ::: "gp_searchProjects"

      -- | Home Page
      ::: "home_pleaseLogInA"
      ::: "home_pleaseLogInB"
      ::: "home_toLogin"
      ::: "home_yourProjects"

      -- | Login Page
      ::: "login_passwordForgotten"

      -- | Navar
      ::: "navbar_documents"
      ::: "navbar_groups"
      ::: "navbar_users"

      -- | 404 Page
      ::: "p404_notFound"

      -- | Profile Page
      ::: "prof_accountEmail"
      ::: "prof_accountEmailHelp"
      ::: "prof_chooseResetMethod"
      ::: "prof_close"
      ::: "prof_confirmPassword"
      ::: "prof_errorOccurred"
      ::: "prof_failedToSaveUsername"
      ::: "prof_featureNotImplemented"
      ::: "prof_groupsAndRoles"
      ::: "prof_loginSuccessful"
      ::: "prof_newPassword"
      ::: "prof_orSeparator"
      ::: "prof_passwordMismatch"
      ::: "prof_passwordResetLinkSent"
      ::: "prof_passwordSecurity"
      ::: "prof_passwordStrengthHelp"
      ::: "prof_passwordUpdated"
      ::: "prof_profile"
      ::: "prof_resetPassword"
      ::: "prof_role"
      ::: "prof_rolesHelp"
      ::: "prof_sendResetLink"
      ::: "prof_unsaved"
      ::: "prof_updatePassword"
      ::: "prof_userData"
      ::: "prof_usernameHelp"
      ::: "prof_usernameSaved"
      ::: "prof_you"

      -- | Reset Password Page
      ::: "rp_ConfirmationCode"
      ::: "rp_Header"
      ::: "rp_InputCode"
      ::: "rp_NoMatch"
      ::: "rp_PasswordConfirm"
      ::: "rp_PasswordNew"
      ::: "rp_RequestCode"

      -- | Table of Contents (TOC)
      ::: "toc_end_dropzone"
      ::: "toc_paragraph"
      ::: "toc_section"

      ::: SNil
  )
