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
import Data.Vector (Vector)
import Hasql.Session (Session, statement)
import qualified UserManagement.Statements as Statements
import qualified UserManagement.User as User
import qualified UserManagement.Group as Group

getUsers :: Session (Vector User.User)
getUsers = statement () Statements.getUsers

getUserID :: Text -> Session User.UserID
getUserID userEmail = statement userEmail Statements.getUserID

getLoginRequirements :: Text -> Session (Maybe (User.UserID, Text))
getLoginRequirements userEmail = statement userEmail Statements.getLoginRequirements

getUser :: Text -> Session (Maybe User.User)
getUser userEmail = statement userEmail Statements.getUser

getAllUserRoles :: User.UserID -> Session [(Group.GroupID, Maybe User.Role)]
getAllUserRoles uid =
    fmap (Data.Bifunctor.second User.textToRole)
        <$> statement uid Statements.getAllUserRoles

getUserRoleInGroup :: User.UserID -> Group.GroupID -> Session (Maybe User.Role)
getUserRoleInGroup uid group =
    maybe Nothing User.textToRole
        <$> statement (uid, group) Statements.getUserRoleInGroup

putUser :: User.User -> Session User.UserID
putUser user = statement user Statements.putUser

deleteUser :: User.UserID -> Session ()
deleteUser uid = statement uid Statements.deleteUser

updateUserName :: Text -> User.UserID -> Session ()
updateUserName name uid = statement (name, uid) Statements.updateUserName

updateUserEmail :: Text -> User.UserID -> Session ()
updateUserEmail email uid = statement (email, uid) Statements.updateUserName

updateUserPWHash :: Text -> User.UserID -> Session ()
updateUserPWHash pwhash uid = statement (pwhash, uid) Statements.updateUserName

addGroup :: Text -> Maybe Text -> Session Group.GroupID
addGroup group description = statement (group, description) Statements.addGroup

addRole :: User.UserID -> Group.GroupID -> User.Role -> Session ()
addRole uid gid role =
    let sqlrole = User.roleToText role
     in statement (uid, gid, sqlrole) Statements.addRole

updateUserRoleInGroup :: User.UserID -> Group.GroupID -> User.Role -> Session ()
updateUserRoleInGroup uid gid role = let roletext = User.roleToText role
                                      in statement (uid, gid, roletext) Statements.updateUserRoleInGroup

removeUserFromGroup :: User.UserID -> Group.GroupID  -> Session ()
removeUserFromGroup uid gid = statement (uid, gid) Statements.removeUserFromGroup

getMembersOfGroup :: Group.GroupID -> Session [User.UserInfo]
getMembersOfGroup group_id = statement group_id Statements.getMembersOfGroup