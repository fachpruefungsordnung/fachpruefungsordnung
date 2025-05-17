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
    )
where

import Data.Profunctor (lmap, rmap)
import Data.Text
import Data.Tuple.Curry (uncurryN)
import Data.UUID
import Data.Vector
import GHC.Int
import Hasql.Statement
import Hasql.TH
import qualified UserManagement.User as User
import Data.Maybe (listToMaybe)

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

getUserRoleInGroup :: Statement (UUID, Text) (Maybe Text)
getUserRoleInGroup = 
  rmap
      (listToMaybe . toList) 
      [vectorStatement|
        select
          r.role :: text
        from users u
        join roles r on u.id = r.user_id
        join groups g on g.id = r.group_id
        where u.id = $1 :: uuid and g.name = $2 :: text
      |]

getUsers :: Statement () (Vector User.User)
getUsers =
    rmap
        (fmap (uncurryN User.User))
        [vectorStatement|
      select name :: text, email :: text, pwhash :: text
      from users
    |]

getAllUserRoles :: Statement UUID [(Text, Text)]
getAllUserRoles = rmap toList
  [vectorStatement|
    select g.name :: text, r.role :: text
    from users u
    join roles r on u.id = r.user_id
    join groups g on g.id = r.group_id
    where u.id = $1 :: uuid
  |]


putUser :: Statement User.User Int32
putUser =
    lmap
        (\(User.User name email pwhash) -> (name, email, pwhash))
        [singletonStatement|
      insert into users (name, email, pwhash)
      values ($1 :: text, $2 :: text, $3 :: text)
      returning id :: int4
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
        [singletonStatement|
      insert into roles (user_id, group_id, role)
      values ($1 :: uuid, $2 :: int4, $3 :: text)
    |]
