module UserManagement.Transactions (checkSuperadmin) where

import Hasql.Transaction (Transaction, statement)

import qualified UserManagement.Statements as Statements
import UserManagement.User (UserID)

checkSuperadmin :: UserID -> Transaction Bool
checkSuperadmin uid = statement uid Statements.checkSuperadmin
