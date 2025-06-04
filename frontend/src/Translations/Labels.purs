module Translations.Labels where

import Data.Array (foldr)
import Data.Function (($))
import Data.Functor (map)
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (merge)
import Record.Extra (type (:::), SCons, SList, SNil)
import Simple.I18n.Translation (Translation, fromRecord, toRecord)
import Translations.Common (deCommon, enCommon)
import Translations.ResetPassword (dePasswordReset, enPasswordReset)
import Type.Data.Ordering (class Append) as RL

-- | All kinds of abstract labels representing UI texts,
-- | detached from the actual language selection.
-- |
-- | Symbols MUST be in alphabetic order.
-- | Because of this constraint, it's sensible to use
-- | appropriate prefixes for strongly related labels.
type Labels =
  ( "email"
      ::: "emailAddress"
      ::: "home"
      ::: "loginSuccessful"
      ::: "password"
      ::: "passwordForgotten"
      ::: "profile"
      ::: "role"

      -- | Reset Password Page
      ::: "rp_ConfirmationCode"
      ::: "rp_Header"
      ::: "rp_InputCode"
      ::: "rp_NoMatch"
      ::: "rp_PasswordConfirm"
      ::: "rp_PasswordNew"
      ::: "rp_RequestCode"

      ::: "submit"
      ::: "userData"
      ::: "userName"
      ::: SNil
  )

-- | Übersetzungen zusammenführen
en :: Translation Labels
en = fromRecord $ merge (merge (toRecord enCommon) (toRecord enAuth))
  (toRecord enPasswordReset)

de :: Translation Labels
de = fromRecord
  (merge (merge (toRecord deCommon) (toRecord deAuth)) (toRecord dePasswordReset))
