module FPO.Translations.Labels where

import Data.Function (($))
import FPO.Translations.Common (deCommon, enCommon)
import FPO.Translations.Components.Editor (deEditor, enEditor)
import FPO.Translations.Components.Navbar (deNavbar, enNavbar)
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

-- | Übersetzungen zusammenführen
en :: Translation Labels
en = fromRecord $
  merge
    ( merge
        ( merge
            ( merge (toRecord enAdminPanel)
                (toRecord enAdminGroupPage)
            )
            ( merge (toRecord enAdminUserPage)
                (toRecord enCommon)
            )
        )
        ( merge
            ( merge (toRecord enEditor)
                (toRecord enNavbar)
            )
            ( merge (toRecord enHome)
                (toRecord enLogin)
            )
        )
    )

    ( merge
        ( merge
            ( merge (toRecord enPage404)
                (toRecord enPasswordReset)
            )
            ( merge (toRecord enProfile)
                (toRecord enGroupProjectsPage)
            )
        )
        ( merge
            (merge {} {})
            (merge {} {})
        )
    )

de :: Translation Labels
de = fromRecord $
  merge
    ( merge
        ( merge
            ( merge (toRecord deAdminPanel)
                (toRecord deAdminGroupPage)
            )
            ( merge (toRecord deAdminUserPage)
                (toRecord deCommon)
            )
        )
        ( merge
            ( merge (toRecord deEditor)
                (toRecord deNavbar)
            )
            ( merge (toRecord deHome)
                (toRecord deLogin)
            )
        )
    )

    ( merge
        ( merge
            ( merge (toRecord dePage404)
                (toRecord dePasswordReset)
            )
            ( merge (toRecord deProfile)
                (toRecord deGroupProjectsPage)
            )
        )
        ( merge
            (merge {} {})
            (merge {} {})
        )
    )

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

      -- | Admin Users Page
      ::: "admin_users_create"
      ::: "admin_users_createNewUser"
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
      ::: "common_create"
      ::: "common_delete"
      ::: "common_deletePhraseA"
      ::: "common_deletePhraseB"
      ::: "common_email"
      ::: "common_emailAddress"
      ::: "common_filterBy"
      ::: "common_group"
      ::: "common_home"
      ::: "common_members"
      ::: "common_password"
      ::: "common_project"
      ::: "common_submit"
      ::: "common_theGroup"
      ::: "common_userName"

      -- | Editor Page
      ::: "editor_comment"
      ::: "editor_deleteComment"
      ::: "editor_fontSizeDown"
      ::: "editor_fontSizeUp"
      ::: "editor_redo"
      ::: "editor_textBold"
      ::: "editor_textItalic"
      ::: "editor_textUnderline"
      ::: "editor_undo"

      -- | Group Projects Page

      ::: "gp_groupProjects"
      ::: "gp_newProject"
      ::: "gp_projectManagement"
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
      ::: "prof_loginSuccessful"
      ::: "prof_profile"
      ::: "prof_role"
      ::: "prof_userData"

      -- | Reset Password Page
      ::: "rp_ConfirmationCode"
      ::: "rp_Header"
      ::: "rp_InputCode"
      ::: "rp_NoMatch"
      ::: "rp_PasswordConfirm"
      ::: "rp_PasswordNew"
      ::: "rp_RequestCode"

      ::: SNil
  )
