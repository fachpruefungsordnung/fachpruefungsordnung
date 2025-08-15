module FPO.Dto.UserDto where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array (any, filter)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FPO.Dto.UserRoleDto as UR

type UserID = String

-- | Representation of a user in the application,
-- | based on the API's `GET /me` endpoint.
newtype FullUserDto = FullUserDto
  { fullUserEmail :: String
  , fullUserID :: UserID
  , fullUserIsSuperadmin :: Boolean
  , fullUserName :: String
  , fullUserRoles :: Array UR.FullUserRoleDto
  }

getUserEmail :: FullUserDto -> String
getUserEmail (FullUserDto u) = u.fullUserEmail

getUserID :: FullUserDto -> UserID
getUserID (FullUserDto u) = u.fullUserID

isUserSuperadmin :: FullUserDto -> Boolean
isUserSuperadmin (FullUserDto u) = u.fullUserIsSuperadmin

getFullUserRoles :: FullUserDto -> Array UR.FullUserRoleDto
getFullUserRoles (FullUserDto u) = u.fullUserRoles

-- | Checks if the user is admin of at least one group, or even a superadmin.
isAdmin :: FullUserDto -> Boolean
isAdmin (FullUserDto u) = u.fullUserIsSuperadmin || any
  (\role -> UR.getUserRole role == UR.Admin)
  u.fullUserRoles

-- | Checks if the user is an admin of a specific group.
-- | Returns `true` if the user is an admin of the group with the given ID,
-- | detached from the superadmin state of the user.
-- |
-- | See `isUserSuperadmin` for checking if the user is a superadmin.
isAdminOf :: FullUserDto -> Int -> Boolean
isAdminOf (FullUserDto u) groupID =
  any (\role -> UR.getGroupID role == groupID && UR.getUserRole role == UR.Admin)
    u.fullUserRoles

-- | Returns all admin roles of the user.
getAllAdminRoles :: FullUserDto -> Array UR.FullUserRoleDto
getAllAdminRoles (FullUserDto u) =
  filter (\role -> UR.getUserRole role == UR.Admin) u.fullUserRoles

getUserName :: FullUserDto -> String
getUserName (FullUserDto u) = u.fullUserName

getUserRoles :: FullUserDto -> Array UR.FullUserRoleDto
getUserRoles (FullUserDto u) = u.fullUserRoles

derive instance newtypeFullUserDto :: Newtype FullUserDto _
derive newtype instance encodeJsonFullUserDto :: EncodeJson FullUserDto
derive newtype instance decodeJsonFullUserDto :: DecodeJson FullUserDto
derive instance genericFullUserDto :: Generic FullUserDto _
instance showFullUserDto :: Show FullUserDto where
  show = genericShow

-- This is for the PATCH /users/:id endpoint
newtype PatchUserDto = PatchUserDto
  { newEmail :: String
  , newName :: String
  }

derive instance newtypePatchUserDto :: Newtype PatchUserDto _
derive newtype instance encodeJsonPatchUserDto :: EncodeJson PatchUserDto
derive newtype instance decodeJsonPatchUserDto :: DecodeJson PatchUserDto
derive instance genericPatchUserDto :: Generic PatchUserDto _
instance showPatchUserDto :: Show PatchUserDto where
  show = genericShow
