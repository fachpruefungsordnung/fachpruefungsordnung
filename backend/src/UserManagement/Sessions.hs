module UserManagement.Sessions
    ( getUsers
    , getUserByEmail
    , getUserByID
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
    , deleteGroup
    , addRole
    , updateUserRoleInGroup
    , removeUserFromGroup
    , getMembersOfGroup
    , addSuperadmin
    , removeSuperadmin
    , checkSuperadmin
    )
where

import qualified Data.Bifunctor (second)
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Session (Session, statement)
import qualified UserManagement.Group as Group
import qualified UserManagement.Statements as Statements
import qualified UserManagement.User as User

getUsers :: Session (Vector User.User)
getUsers = statement () Statements.getUsers

getUserID :: Text -> Session User.UserID
getUserID userEmail = statement userEmail Statements.getUserID

getLoginRequirements :: Text -> Session (Maybe (User.UserID, Text))
getLoginRequirements userEmail = statement userEmail Statements.getLoginRequirements

getUserByEmail :: Text -> Session (Maybe User.User)
getUserByEmail userEmail = statement userEmail Statements.getUserByEmail

getUserByID :: User.UserID -> Session (Maybe User.User)
getUserByID userID = statement userID Statements.getUserByID

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

updateUserName :: User.UserID -> Text -> Session ()
updateUserName uid name = statement (name, uid) Statements.updateUserName

updateUserEmail :: User.UserID -> Text -> Session ()
updateUserEmail uid email = statement (email, uid) Statements.updateUserEmail

updateUserPWHash :: User.UserID -> Text -> Session ()
updateUserPWHash uid pwhash = statement (pwhash, uid) Statements.updateUserName

addGroup :: Text -> Maybe Text -> Session Group.GroupID
addGroup group description = statement (group, description) Statements.addGroup

deleteGroup :: Group.GroupID -> Session ()
deleteGroup groupID = statement groupID Statements.deleteGroup

addRole :: User.UserID -> Group.GroupID -> User.Role -> Session ()
addRole uid gid role =
    let sqlrole = User.roleToText role
     in statement (uid, gid, sqlrole) Statements.addRole

updateUserRoleInGroup :: User.UserID -> Group.GroupID -> User.Role -> Session ()
updateUserRoleInGroup uid gid role =
    let roletext = User.roleToText role
     in statement (uid, gid, roletext) Statements.updateUserRoleInGroup

removeUserFromGroup :: User.UserID -> Group.GroupID -> Session ()
removeUserFromGroup uid gid = statement (uid, gid) Statements.removeUserFromGroup

getMembersOfGroup :: Group.GroupID -> Session [User.UserInfo]
getMembersOfGroup group_id = statement group_id Statements.getMembersOfGroup

addSuperadmin :: User.UserID -> Session ()
addSuperadmin uid = statement uid Statements.addSuperadmin

removeSuperadmin :: User.UserID -> Session ()
removeSuperadmin uid = statement uid Statements.removeSuperadmin

checkSuperadmin :: User.UserID -> Session Bool
checkSuperadmin uid = statement uid Statements.checkSuperadmin
