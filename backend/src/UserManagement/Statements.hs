{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UserManagement.Statements
    ( getUsers
    , getUserByEmail
    , getUserByID
    , putUser
    , deleteUser
    , getUserID
    , getLoginRequirements
    , getUserRoleInGroup
    , getAllUserRoles
    , updateUserName
    , updateUserEmail
    , updateUserPWHash
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
    )
where

import Data.Maybe (listToMaybe)
import Data.Profunctor (lmap, rmap)
import Data.Text
import Data.Tuple.Curry (uncurryN)
import Data.Vector
import GHC.Int
import Hasql.Statement
import Hasql.TH
import UserManagement.Document (textToPermission)
import qualified UserManagement.Document as Document
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
     select name :: text, email :: text, pwhash :: text
     from users
     where email = $1 :: text
   |]

getUserByID :: Statement User.UserID (Maybe User.User)
getUserByID =
    rmap
        (fmap (uncurryN User.User))
        [maybeStatement|
     select name :: text, email :: text, pwhash :: text
     from users
     where id = $1 :: uuid
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

getUsers :: Statement () (Vector User.User)
getUsers =
    rmap
        (fmap (uncurryN User.User))
        [vectorStatement|
      select name :: text, email :: text, pwhash :: text
      from users
    |]

getAllUserRoles :: Statement User.UserID [(Group.GroupID, Text)]
getAllUserRoles =
    rmap
        toList
        [vectorStatement|

    select g.id :: int4, r.role :: text
    from users u
    join roles r on u.id = r.user_id
    join groups g on g.id = r.group_id
    where u.id = $1 :: uuid
  |]

putUser :: Statement User.User User.UserID
putUser =
    lmap
        (\(User.User name email pwhash) -> (name, email, pwhash))
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
checkGroupDocPermission :: Statement (User.UserID, Document.DocumentID) Bool
checkGroupDocPermission =
    [singletonStatement|
      
      select exists (
        select 1
        from roles r
        join documents d on d.group_id = r.group_id
        where r.user_id = $1 :: uuid and d.id = $2 :: int4
      ) :: bool
    |]

-- | extract the DocPermission for external document editors if they exist
getExternalDocPermission
    :: Statement (User.UserID, Document.DocumentID) (Maybe Document.DocPermission)
getExternalDocPermission =
    rmap
        (>>= textToPermission)
        [maybeStatement|
    select permission :: text 
    from external_document_rights
    where user_id = $1 :: uuid and document_id = $2 :: int4
  |]
