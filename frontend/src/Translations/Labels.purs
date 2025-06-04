module Translations.Labels where

import Data.Function (($))
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (merge)
import Record.Extra (type (:::), SCons, SNil)
import Simple.I18n.Translation (Translation, fromRecord, toRecord)
import Translations.Common (deCommon, enCommon)
import Translations.Login (deLogin, enLogin)
import Translations.Profile (deProfile, enProfile)
import Translations.ResetPassword (dePasswordReset, enPasswordReset)
import Type.Data.Ordering (class Append) as RL

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

-- | Übersetzungen zusammenführen
en :: Translation Labels
en = fromRecord $
  merge
    (merge (toRecord enCommon) (toRecord enLogin))
    (merge (toRecord enPasswordReset) (toRecord enProfile))

de :: Translation Labels
de = fromRecord $
  merge
    (merge (toRecord deCommon) (toRecord deLogin))
    (merge (toRecord dePasswordReset) (toRecord deProfile))
