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
    , checkGroupMembership
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
    , checkGroupDocPermission
    , getExternalDocPermission
    , getDocumentGroupID
    , getAllExternalUsersOfDocument
    , addExternalDocPermission
    , updateExternalDocPermission
    , deleteExternalDocPermission
    )
where

import qualified Data.Bifunctor (second)
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Session (Session, statement)
import qualified UserManagement.Document as Document
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

checkGroupMembership :: User.UserID -> Group.GroupID -> Session Bool
checkGroupMembership userID groupID = statement (userID, groupID) Statements.checkGroupMembership

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

checkGroupDocPermission :: User.UserID -> Document.DocumentID -> Session Bool
checkGroupDocPermission uid did = statement (uid, did) Statements.checkGroupDocPermission

getExternalDocPermission
    :: User.UserID -> Document.DocumentID -> Session (Maybe Document.DocPermission)
getExternalDocPermission uid did = statement (uid, did) Statements.getExternalDocPermission

getDocumentGroupID :: Document.DocumentID -> Session (Maybe Group.GroupID)
getDocumentGroupID did = statement did Statements.getDocumentGroupID

getAllExternalUsersOfDocument
    :: Document.DocumentID -> Session [(User.UserID, Document.DocPermission)]
getAllExternalUsersOfDocument did = do
    users <- statement did Statements.getAllExternalUsersOfDocument
    return [(user, perm) | (user, Just perm) <- users]

addExternalDocPermission
    :: User.UserID -> Document.DocumentID -> Document.DocPermission -> Session ()
addExternalDocPermission uid did perm =
    let perm' = Document.permissionToText perm
     in statement (uid, did, perm') Statements.addExternalDocPermission

updateExternalDocPermission
    :: User.UserID -> Document.DocumentID -> Document.DocPermission -> Session ()
updateExternalDocPermission uid did perm =
    let perm' = Document.permissionToText perm
     in statement (uid, did, perm') Statements.updateExternalDocPermission

deleteExternalDocPermission :: User.UserID -> Document.DocumentID -> Session ()
deleteExternalDocPermission uid did = statement (uid, did) Statements.deleteExternalDocPermission
