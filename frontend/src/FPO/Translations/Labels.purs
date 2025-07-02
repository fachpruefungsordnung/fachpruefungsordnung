module FPO.Translations.Labels where

import Data.Function (($))
import FPO.Translations.Common (deCommon, enCommon)
import FPO.Translations.Components.Editor (deEditor, enEditor)
import FPO.Translations.Components.Navbar (deNavbar, enNavbar)
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
  merge (toRecord enAdminPanel)
    ( merge
        ( merge (merge (toRecord enAdminUserPage) (toRecord enCommon))
            (merge (toRecord enEditor) (merge (toRecord enNavbar) (toRecord enHome)))
        )
        ( merge
            (merge (toRecord enLogin) (toRecord enPage404))
            (merge (toRecord enPasswordReset) (toRecord enProfile))
        )
    )

de :: Translation Labels
de = fromRecord $
  merge (toRecord deAdminPanel)
    ( merge
        ( merge (merge (toRecord deAdminUserPage) (toRecord deCommon))
            (merge (toRecord deEditor) (merge (toRecord deNavbar) (toRecord deHome)))
        )
        ( merge
            (merge (toRecord deLogin) (toRecord dePage404))
            (merge (toRecord dePasswordReset) (toRecord deProfile))
        )
    )

-- | All kinds of abstract labels representing UI texts,
-- | detached from the actual language selection.
-- |
-- | Symbols MUST be in alphabetic order.
-- | Because of this constraint, it's sensible to use
-- | appropriate prefixes for strongly related labels.
type Labels =
  ( "admin_users_create"
      ::: "admin_users_createNewUser"
      ::: "admin_users_failedToCreateUser"
      ::: "admin_users_failedToLoadUsers"
      ::: "admin_users_goToProfilePage"
      ::: "admin_users_listOfUsers"
      ::: "admin_users_successfullyCreatedUser"

      -- | Admin Panel
      ::: "au_documentManagement"
      ::: "au_groupDocuments"
      ::: "au_groupManagement"
      ::: "au_userManagement"

      -- | Common Phrases
      ::: "common_email"
      ::: "common_emailAddress"
      ::: "common_filterBy"
      ::: "common_home"
      ::: "common_password"
      ::: "common_submit"
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
