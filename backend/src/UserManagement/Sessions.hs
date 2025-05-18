module UserManagement.Sessions
    ( getUsers
    , getUser
    , getUserID
    , putUser
    , getUserRoleInGroup
    , getLoginRequirements
    , getAllUserRoles
    , addGroup
    , addRole
    )
where

import Data.Text (Text)
import Data.UUID
import Data.Vector (Vector)
import GHC.Int
import Hasql.Session (Session, statement)
import qualified UserManagement.Statements as Statements
import qualified UserManagement.User as User
import qualified Data.Bifunctor (second)

getUsers :: Session (Vector User.User)
getUsers = statement () Statements.getUsers

getUserID :: Text -> Session UUID
getUserID userEmail = statement userEmail Statements.getUserID

getLoginRequirements :: Text -> Session (Maybe (UUID, Text))
getLoginRequirements userEmail = statement userEmail Statements.getLoginRequirements

getUser :: Text -> Session (Maybe User.User)
getUser userEmail = statement userEmail Statements.getUser

getAllUserRoles :: UUID -> Session [(Int32, Maybe User.Role)]
getAllUserRoles uid = fmap (Data.Bifunctor.second User.textToRole)
                                <$> statement uid Statements.getAllUserRoles

getUserRoleInGroup :: UUID -> Int32 -> Session (Maybe User.Role)
getUserRoleInGroup uid group = maybe Nothing User.textToRole
                                <$> statement (uid, group) Statements.getUserRoleInGroup

putUser :: User.User -> Session UUID
putUser user = statement user Statements.putUser

addGroup :: Text -> Maybe Text -> Session Int32
addGroup group description = statement (group, description) Statements.addGroup

addRole :: UUID -> Int32 -> User.Role -> Session ()
addRole uid gid role = let sqlrole = User.roleToText role in
                statement (uid, gid, sqlrole) Statements.addRole
