module UserManagement.Sessions
    ( getAllUsers
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
    , getGroupInfo
    , getAllGroupsOverview
    , deleteGroup
    , addRole
    , updateUserRoleInGroup
    , removeUserFromGroup
    , getMembersOfGroup
    , addSuperadmin
    , removeSuperadmin
    , checkSuperadmin
    , checkGroupPermission
    , checkGroupNameExistence
    , getExternalPermission
    , getDocumentGroupID
    , getAllExternalUsersOfDocument
    , addExternalPermission
    , updateExternalPermission
    , deleteExternalPermission
    , getAllVisibleDocuments
    , getAllDocumentsOfGroup
    )
where

import qualified Data.Bifunctor (second)
import Data.Text (Text)
import qualified DocumentManagement.Document as Document
import Hasql.Session (Session, statement)
import qualified UserManagement.DocumentPermission as Permission
import qualified UserManagement.Group as Group
import qualified UserManagement.Statements as Statements
import qualified UserManagement.User as User

getAllUsers :: Session [User.User]
getAllUsers = statement () Statements.getAllUsers

getUserID :: Text -> Session User.UserID
getUserID userEmail = statement userEmail Statements.getUserID

getLoginRequirements :: Text -> Session (Maybe (User.UserID, Text))
getLoginRequirements userEmail = statement userEmail Statements.getLoginRequirements

getUserByEmail :: Text -> Session (Maybe User.User)
getUserByEmail userEmail = statement userEmail Statements.getUserByEmail

getUserByID :: User.UserID -> Session (Maybe User.User)
getUserByID userID = statement userID Statements.getUserByID

getAllUserRoles :: User.UserID -> Session [(Group.GroupID, Text, Maybe User.Role)]
getAllUserRoles uid =
    fmap (Data.Bifunctor.second User.textToRole)
        <$> statement uid Statements.getAllUserRoles

checkGroupMembership :: User.UserID -> Group.GroupID -> Session Bool
checkGroupMembership userID groupID = statement (userID, groupID) Statements.checkGroupMembership

getUserRoleInGroup :: User.UserID -> Group.GroupID -> Session (Maybe User.Role)
getUserRoleInGroup uid group =
    maybe Nothing User.textToRole
        <$> statement (uid, group) Statements.getUserRoleInGroup

putUser :: User.UserCreate -> Session User.UserID
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

-- | returns name and description of specified group
getGroupInfo :: Group.GroupID -> Session Group.GroupCreate
getGroupInfo groupID = statement groupID Statements.getGroupInfo

getAllGroupsOverview :: Session [Group.GroupOverview]
getAllGroupsOverview = statement () Statements.getAllGroupsOverview

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

checkGroupPermission :: User.UserID -> Document.DocumentID -> Session Bool
checkGroupPermission uid did = statement (uid, did) Statements.checkGroupPermission

checkGroupNameExistence :: Text -> Session Bool
checkGroupNameExistence name = statement name Statements.checkGroupNameExistence

getExternalPermission
    :: User.UserID -> Document.DocumentID -> Session (Maybe Permission.Permission)
getExternalPermission uid did = statement (uid, did) Statements.getExternalPermission

getDocumentGroupID :: Document.DocumentID -> Session (Maybe Group.GroupID)
getDocumentGroupID did = statement did Statements.getDocumentGroupID

getAllExternalUsersOfDocument
    :: Document.DocumentID -> Session [(User.UserID, Permission.Permission)]
getAllExternalUsersOfDocument did = do
    users <- statement did Statements.getAllExternalUsersOfDocument
    return [(user, perm) | (user, Just perm) <- users]

addExternalPermission
    :: User.UserID -> Document.DocumentID -> Permission.Permission -> Session ()
addExternalPermission uid did perm =
    let perm' = Permission.permissionToText perm
     in statement (uid, did, perm') Statements.addExternalPermission

updateExternalPermission
    :: User.UserID -> Document.DocumentID -> Permission.Permission -> Session ()
updateExternalPermission uid did perm =
    let perm' = Permission.permissionToText perm
     in statement (uid, did, perm') Statements.updateExternalPermission

deleteExternalPermission :: User.UserID -> Document.DocumentID -> Session ()
deleteExternalPermission uid did = statement (uid, did) Statements.deleteExternalPermission

getAllVisibleDocuments :: User.UserID -> Session [Document.Document]
getAllVisibleDocuments uid = statement uid Statements.getAllVisibleDocuments

getAllDocumentsOfGroup :: Group.GroupID -> Session [Document.Document]
getAllDocumentsOfGroup gid = statement gid Statements.getAllDocumentsOfGroup
