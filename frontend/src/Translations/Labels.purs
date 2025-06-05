module FPO.Translations.Labels where

import Data.Function (($))
import FPO.Translations.Common (deCommon, enCommon)
import FPO.Translations.Home (deHome, enHome)
import FPO.Translations.Login (deLogin, enLogin)
import FPO.Translations.Profile (deProfile, enProfile)
import FPO.Translations.ResetPassword (dePasswordReset, enPasswordReset)
import Record (merge)
import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord, toRecord)
import Translations.Page.Page404 (dePage404, enPage404)

-- | Übersetzungen zusammenführen
en :: Translation Labels
en = fromRecord $
  merge
    (merge (toRecord enCommon) (toRecord enHome))
    ( merge
        (merge (toRecord enLogin) (toRecord enPage404))
        (merge (toRecord enPasswordReset) (toRecord enProfile))
    )

de :: Translation Labels
de = fromRecord $
  merge
    (merge (toRecord deCommon) (toRecord deHome))
    ( merge
        (merge (toRecord deLogin) (toRecord dePage404))
        (merge (toRecord dePasswordReset) (toRecord deProfile))
    )

-- | All kinds of abstract labels representing UI texts,
-- | detached from the actual language selection.
-- |
-- | Symbols MUST be in alphabetic order.
-- | Because of this constraint, it's sensible to use
-- | appropriate prefixes for strongly related labels.
type Labels =
  ( -- | Common Phrases
    "common_email"
      ::: "common_emailAddress"
      ::: "common_home"
      ::: "common_password"
      ::: "common_submit"

      -- | Home Page
      ::: "home_pleaseLogInA"
      ::: "home_pleaseLogInB"
      ::: "home_toLogin"
      ::: "home_yourProjects"

      -- | Login Page
      ::: "login_passwordForgotten"

      -- | 404 Page
      ::: "p404_notFound"

      -- | Profile Page
      ::: "prof_loginSuccessful"
      ::: "prof_profile"
      ::: "prof_role"
      ::: "prof_userData"
      ::: "prof_userName"

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
