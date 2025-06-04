module Translations.Labels where

import Data.Function (($))
import Record (merge)
import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord, toRecord)
import Translations.Common (deCommon, enCommon)
import Translations.Home (deHome, enHome)
import Translations.Login (deLogin, enLogin)
import Translations.Profile (deProfile, enProfile)
import Translations.ResetPassword (dePasswordReset, enPasswordReset)

-- | Übersetzungen zusammenführen
en :: Translation Labels
en = fromRecord $
  merge
    (merge (toRecord enCommon) (toRecord enLogin))
    ( merge
        (toRecord enPasswordReset)
        (merge (toRecord enProfile) (toRecord enHome))
    )

de :: Translation Labels
de = fromRecord $
  merge
    (merge (toRecord deCommon) (toRecord deLogin))
    ( merge
        (toRecord dePasswordReset)
        (merge (toRecord deProfile) (toRecord deHome))
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
      ::: "home_pleaseLogIn"
      ::: "home_toLogin"
      ::: "home_yourProjects"

      -- | Login Page
      ::: "login_passwordForgotten"

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
