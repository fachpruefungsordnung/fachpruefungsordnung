{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UserManagement.Statements
    ( getUsers
    , getUser
    , putUser
    , getUserID
    , getLoginRequirements
    , getUserRoleInGroup
    , getAllUserRoles
    , addGroup
    , addRole
    , getMembersOfGroup
    )
where

import Data.Maybe (listToMaybe)
import Data.Profunctor (lmap, rmap)
import Data.Text
import Data.Tuple.Curry (uncurryN)
import Data.UUID
import Data.Vector
import GHC.Int
import Hasql.Statement
import Hasql.TH
import qualified UserManagement.User as User
import Prelude hiding (id)

getUserID :: Statement Text UUID
getUserID =
    [singletonStatement|
    select
      id :: uuid
    from
      users
    where
      email = $1 :: text
  |]

getLoginRequirements :: Statement Text (Maybe (UUID, Text))
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

getUser :: Statement Text (Maybe User.User)
getUser =
    rmap
        (fmap (uncurryN User.User))
        [maybeStatement|
     select name :: text, email :: text, pwhash :: text
     from users
     where email = $1 :: text
   |]

getUserRoleInGroup :: Statement (UUID, Int32) (Maybe Text)
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

getAllUserRoles :: Statement UUID [(Int32, Text)]
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

putUser :: Statement User.User UUID
putUser =
    lmap
        (\(User.User name email pwhash) -> (name, email, pwhash))
        [singletonStatement|
      insert into users (name, email, pwhash)
      values ($1 :: text, $2 :: text, $3 :: text)
      returning id :: uuid
    |]

addGroup :: Statement (Text, Maybe Text) Int32
addGroup =
    [singletonStatement|

      insert into groups (name, description)
      values ($1 :: text, $2 :: text?)
      returning id :: int4
    |]

addRole :: Statement (UUID, Int32, Text) ()
addRole =
    [resultlessStatement|

      insert into roles (user_id, group_id, role)
      values ($1 :: uuid, $2 :: int4, $3 :: text)
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
