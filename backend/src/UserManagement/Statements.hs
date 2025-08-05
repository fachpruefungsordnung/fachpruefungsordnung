{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UserManagement.Statements
    ( getAllUsers
    , getUserByEmail
    , getUserByID
    , putUser
    , deleteUser
    , getUserID
    , getLoginRequirements
    , checkGroupMembership
    , getUserRoleInGroup
    , getAllUserRoles
    , updateUserName
    , updateUserEmail
    , updateUserPWHash
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

import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (listToMaybe)
import Data.Profunctor (lmap, rmap)
import Data.Text
import Data.Tuple.Curry (uncurryN)
import Data.Vector
import qualified DocumentManagement.Commit as Commit
import qualified DocumentManagement.Document as Document
import GHC.Int
import Hasql.Statement
import Hasql.TH
import qualified UserManagement.DocumentPermission as Permission
import qualified UserManagement.Group as Group
import qualified UserManagement.User as User
import Prelude hiding (id)

getUserID :: Statement Text User.UserID
getUserID =
    [singletonStatement|
    select
      id :: uuid
    from
      users
    where
      email = $1 :: text
  |]

getLoginRequirements :: Statement Text (Maybe (User.UserID, Text))
getLoginRequirements =
    rmap
        (listToMaybe . toList)
        [vectorStatement|

        select
          id :: uuid, pwhash :: text
        from
          users
        where
          email = $1 :: text
      |]

getUserByEmail :: Statement Text (Maybe User.User)
getUserByEmail =
    rmap
        (fmap (uncurryN User.User))
        [maybeStatement|
     select id :: uuid, name :: text, email :: text
     from users
     where email = $1 :: text
   |]

getUserByID :: Statement User.UserID (Maybe User.User)
getUserByID =
    rmap
        (fmap (uncurryN User.User))
        [maybeStatement|
     select id :: uuid, name :: text, email :: text
     from users
     where id = $1 :: uuid
   |]

-- | Checks if User has any role in Group and returns True or False
checkGroupMembership :: Statement (User.UserID, Group.GroupID) Bool
checkGroupMembership =
    [singletonStatement|

      select exists (
        select 1
        from roles
        where user_id = $1 :: uuid and group_id = $2 :: int4
      ) :: bool
    |]

getUserRoleInGroup :: Statement (User.UserID, Group.GroupID) (Maybe Text)
getUserRoleInGroup =
    rmap
        (listToMaybe . toList)
        [vectorStatement|

        select
          r.role :: text
        from users u
        join roles r on u.id = r.user_id
        join groups g on g.id = r.group_id
        where u.id = $1 :: uuid and g.id = $2 :: int4
      |]

getAllUsers :: Statement () [User.User]
getAllUsers =
    fmap (\(id, name, email) -> User.User (id :: User.UserID) name email)
        <$> rmap
            toList
            [vectorStatement|
      select id :: uuid, name :: text, email :: text
      from users
    |]

getAllUserRoles :: Statement User.UserID [(Group.GroupID, Text, Text)]
getAllUserRoles =
    rmap
        toList
        [vectorStatement|

    select g.id :: int4, g.name :: text, r.role :: text
    from users u
    join roles r on u.id = r.user_id
    join groups g on g.id = r.group_id
    where u.id = $1 :: uuid
  |]

putUser :: Statement User.UserCreate User.UserID
putUser =
    lmap
        (\(User.UserCreate name email pwhash) -> (name, email, pwhash))
        [singletonStatement|
      insert into users (name, email, pwhash)
      values ($1 :: text, $2 :: text, $3 :: text)
      returning id :: uuid
    |]

deleteUser :: Statement User.UserID ()
deleteUser =
    [resultlessStatement|

    delete from users where id = $1 :: uuid
  |]

updateUserName :: Statement (Text, User.UserID) ()
updateUserName =
    [resultlessStatement|

    update users
    set name = $1 :: text
    where id = $2 :: uuid
  |]

updateUserEmail :: Statement (Text, User.UserID) ()
updateUserEmail =
    [resultlessStatement|

    update users
    set email = $1 :: text
    where id = $2 :: uuid
  |]

updateUserPWHash :: Statement (Text, User.UserID) ()
updateUserPWHash =
    [resultlessStatement|

    update users
    set pwhash = $1 :: text
    where id = $2 :: uuid
  |]

addGroup :: Statement (Text, Maybe Text) Group.GroupID
addGroup =
    [singletonStatement|

      insert into groups (name, description)
      values ($1 :: text, $2 :: text?)
      returning id :: int4
    |]

getGroupInfo :: Statement Group.GroupID Group.GroupCreate
getGroupInfo =
    uncurry Group.GroupCreate
        <$> [singletonStatement|
        select name :: text, description :: text?
        from groups
        where id = $1 :: int4
    |]

getAllGroupsOverview :: Statement () [Group.GroupOverview]
getAllGroupsOverview =
    fmap (\(id, name) -> Group.GroupOverview (id :: Group.GroupID) name)
        <$> rmap
            toList
            [vectorStatement|
        select id :: int4, name :: text
        from groups
    |]

deleteGroup :: Statement Group.GroupID ()
deleteGroup =
    [resultlessStatement|
      delete from groups
      where id = $1 :: int4
    |]

addRole :: Statement (User.UserID, Group.GroupID, Text) ()
addRole =
    [resultlessStatement|

      insert into roles (user_id, group_id, role)
      values ($1 :: uuid, $2 :: int4, $3 :: text :: role)
    |]

updateUserRoleInGroup :: Statement (User.UserID, Group.GroupID, Text) ()
updateUserRoleInGroup =
    [resultlessStatement|
      update roles
      set role = $3 :: text :: role
      where user_id = $1 :: uuid and group_id = $2 :: int4
    |]

removeUserFromGroup :: Statement (User.UserID, Group.GroupID) ()
removeUserFromGroup =
    [resultlessStatement|
      delete from roles
      where user_id = $1 :: uuid and group_id = $2 :: int4
    |]

-- | get all Users that have any role in the given group
getMembersOfGroup :: Statement Int32 [User.UserInfo]
getMembersOfGroup =
    rmap
        ( fmap
            ( \(id, name, email, role) ->
                User.UserInfo id name email (read $ unpack role)
            )
            . toList
        )
        [vectorStatement|
    select u.id :: uuid, u.name :: text, u.email :: text, r.role :: text
    from users u
    join roles r on u.id = r.user_id
    join groups g on g.id = r.group_id
    where g.id = $1 :: int4
  |]

addSuperadmin :: Statement User.UserID ()
addSuperadmin =
    [resultlessStatement|

      insert into superadmins (user_id)
      values ($1 :: uuid)
    |]

removeSuperadmin :: Statement User.UserID ()
removeSuperadmin =
    [resultlessStatement|

      delete from superadmins
      where user_id = $1 :: uuid
    |]

checkSuperadmin :: Statement User.UserID Bool
checkSuperadmin =
    [singletonStatement|

      select exists (
        select 1
        from superadmins
        where user_id = $1 :: uuid
      ) :: bool
    |]

-- | check if User is Member (or Admin) of the group that owns the specified document
checkGroupPermission :: Statement (User.UserID, Document.DocumentID) Bool
checkGroupPermission =
    lmap
        (second Document.unDocumentID)
        [singletonStatement|
            select exists (
                select 1
                from roles r
                join documents d on d.group_id = r.group_id
                where r.user_id = $1 :: uuid and d.id = $2 :: int4
            ) :: bool
        |]

checkGroupNameExistence :: Statement Text Bool
checkGroupNameExistence =
    [singletonStatement|
            select exists (
                select 1
                from groups
                where name = $1 :: text
            ) :: bool
        |]

-- | extract the Permission for external document editors if they exist
getExternalPermission
    :: Statement (User.UserID, Document.DocumentID) (Maybe Permission.Permission)
getExternalPermission =
    rmap
        (>>= Permission.textToPermission)
        $ lmap
            (second Document.unDocumentID)
            [maybeStatement|
                select permission :: text
                from external_document_rights
                where user_id = $1 :: uuid and document_id = $2 :: int4
            |]

-- | get the group id of a given document. the maybe is only technical and should never be Nothing in practice.
getDocumentGroupID :: Statement Document.DocumentID (Maybe Group.GroupID)
getDocumentGroupID =
    lmap
        Document.unDocumentID
        [maybeStatement|
            select group_id :: int4
            from documents
            where id = $1 :: int4
        |]

addExternalPermission
    :: Statement (User.UserID, Document.DocumentID, Text) ()
addExternalPermission =
    lmap
        ( \(user, document, permission) -> (user, Document.unDocumentID document, permission)
        )
        [resultlessStatement|
            insert into external_document_rights (user_id, document_id, permission)
            values ($1 :: uuid, $2 :: int4, $3 :: text :: permission)
        |]

updateExternalPermission
    :: Statement (User.UserID, Document.DocumentID, Text) ()
updateExternalPermission =
    lmap
        ( \(user, document, permission) -> (user, Document.unDocumentID document, permission)
        )
        [resultlessStatement|
            update external_document_rights
            set permission = $3 :: text :: permission
            where user_id = $1 :: uuid and document_id = $2 :: int4
        |]

deleteExternalPermission :: Statement (User.UserID, Document.DocumentID) ()
deleteExternalPermission =
    lmap
        (second Document.unDocumentID)
        [resultlessStatement|
            delete from external_document_rights
            where user_id = $1 :: uuid and document_id = $2 :: int4
        |]

getAllExternalUsersOfDocument
    :: Statement Document.DocumentID [(User.UserID, Maybe Permission.Permission)]
getAllExternalUsersOfDocument =
    rmap
        (fmap (Data.Bifunctor.second Permission.textToPermission) . toList)
        $ lmap
            Document.unDocumentID
            [vectorStatement|
                select user_id :: uuid, permission :: text
                from external_document_rights
                where document_id = $1 :: int4
            |]

getAllVisibleDocuments :: Statement User.UserID [Document.Document]
getAllVisibleDocuments =
    rmap
        ( fmap
            ( \(document, name, groupID, headCommit) ->
                Document.Document
                    (Document.DocumentID document)
                    name
                    groupID
                    (Commit.CommitID <$> headCommit)
            )
            . toList
        )
        [vectorStatement|
      (select
        d.id :: int4,
        d.name :: text,
        d.group_id :: int4,
        d.head :: int4?
      from roles r
      join documents d on d.group_id = r.group_id
      where r.user_id = $1 :: uuid)
      union
      (select
        d.id :: int4,
        d.name :: text,
        d.group_id :: int4,
        d.head :: int4?
      from documents d
      join external_document_rights e on d.id = e.document_id
      where e.user_id = $1 :: uuid)
    |]

getAllDocumentsOfGroup :: Statement Group.GroupID [Document.Document]
getAllDocumentsOfGroup =
    rmap
        ( fmap
            ( \(document, name, groupID, headCommit) ->
                Document.Document
                    (Document.DocumentID document)
                    name
                    groupID
                    (Commit.CommitID <$> headCommit)
            )
            . toList
        )
        [vectorStatement|
      select
        id :: int4,
        name :: text,
        group_id :: int4,
        head :: int4?
      from documents
      where group_id = $1 :: int4
    |]
