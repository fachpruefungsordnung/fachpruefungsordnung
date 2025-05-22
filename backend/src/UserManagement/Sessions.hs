module UserManagement.Sessions
    ( getUsers
    , getUser
    , getUserID
    , putUser
    , deleteUser
    , updateUserName
    , updateUserEmail
    , updateUserPWHash
    , getUserRoleInGroup
    , getLoginRequirements
    , getAllUserRoles
    , addGroup
    , addRole
    , updateUserRoleInGroup
    , removeUserFromGroup
    , getMembersOfGroup
    )
where

import qualified Data.Bifunctor (second)
import Data.Text (Text)
import Data.UUID
import Data.Vector (Vector)
import GHC.Int
import Hasql.Session (Session, statement)
import qualified UserManagement.Statements as Statements
import qualified UserManagement.User as User

getUsers :: Session (Vector User.User)
getUsers = statement () Statements.getUsers

getUserID :: Text -> Session UUID
getUserID userEmail = statement userEmail Statements.getUserID

getLoginRequirements :: Text -> Session (Maybe (UUID, Text))
getLoginRequirements userEmail = statement userEmail Statements.getLoginRequirements

getUser :: Text -> Session (Maybe User.User)
getUser userEmail = statement userEmail Statements.getUser

getAllUserRoles :: UUID -> Session [(Int32, Maybe User.Role)]
getAllUserRoles uid =
    fmap (Data.Bifunctor.second User.textToRole)
        <$> statement uid Statements.getAllUserRoles

getUserRoleInGroup :: UUID -> Int32 -> Session (Maybe User.Role)
getUserRoleInGroup uid group =
    maybe Nothing User.textToRole
        <$> statement (uid, group) Statements.getUserRoleInGroup

putUser :: User.User -> Session UUID
putUser user = statement user Statements.putUser

deleteUser :: UUID -> Session ()
deleteUser uid = statement uid Statements.deleteUser

updateUserName :: Text -> UUID -> Session ()
updateUserName name uid = statement (name, uid) Statements.updateUserName

updateUserEmail :: Text -> UUID -> Session ()
updateUserEmail email uid = statement (email, uid) Statements.updateUserName

updateUserPWHash :: Text -> UUID -> Session ()
updateUserPWHash pwhash uid = statement (pwhash, uid) Statements.updateUserName

addGroup :: Text -> Maybe Text -> Session Int32
addGroup group description = statement (group, description) Statements.addGroup

addRole :: UUID -> Int32 -> User.Role -> Session ()
addRole uid gid role =
    let sqlrole = User.roleToText role
     in statement (uid, gid, sqlrole) Statements.addRole

updateUserRoleInGroup :: UUID -> Int32 -> User.Role -> Session ()
updateUserRoleInGroup uid gid role = let roletext = User.roleToText role
                                      in statement (uid, gid, roletext) Statements.updateUserRoleInGroup

removeUserFromGroup :: UUID -> Int32  -> Session ()
removeUserFromGroup uid gid = statement (uid, gid) Statements.removeUserFromGroup

getMembersOfGroup :: Int32 -> Session [User.UserInfo]
getMembersOfGroup group_id = statement group_id Statements.getMembersOfGroup