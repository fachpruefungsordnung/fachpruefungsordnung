module FPO.Dto.UserDto where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  , encodeJson
  )
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

type UserID = String

-- | Representation of a user in the application,
-- | based on the API's `GET /me` endpoint.
newtype FullUserDto = FullUserDto
  { fullUserEmail :: String
  , fullUserID :: UserID
  , fullUserIsSuperadmin :: Boolean
  , fullUserName :: String
  , fullUserRoles :: Array FullUserRoleDto
  }

getUserEmail :: FullUserDto -> String
getUserEmail (FullUserDto u) = u.fullUserEmail

getUserID :: FullUserDto -> UserID
getUserID (FullUserDto u) = u.fullUserID

isUserSuperadmin :: FullUserDto -> Boolean
isUserSuperadmin (FullUserDto u) = u.fullUserIsSuperadmin

getUserName :: FullUserDto -> String
getUserName (FullUserDto u) = u.fullUserName

getUserRoles :: FullUserDto -> Array FullUserRoleDto
getUserRoles (FullUserDto u) = u.fullUserRoles

derive instance newtypeFullUserDto :: Newtype FullUserDto _
derive newtype instance encodeJsonFullUserDto :: EncodeJson FullUserDto
derive newtype instance decodeJsonFullUserDto :: DecodeJson FullUserDto
derive instance genericFullUserDto :: Generic FullUserDto _
instance showFullUserDto :: Show FullUserDto where
  show = genericShow

newtype FullUserRoleDto = FullUserRoleDto
  { groupID :: Int
  , role :: Role
  }

getUserRoleGroupID :: FullUserRoleDto -> Int
getUserRoleGroupID (FullUserRoleDto r) = r.groupID

getUserRole :: FullUserRoleDto -> Role
getUserRole (FullUserRoleDto r) = r.role

derive instance newtypeFullUserRoleDto :: Newtype FullUserRoleDto _
derive newtype instance encodeJsonFullUserRoleDto :: EncodeJson FullUserRoleDto
derive newtype instance decodeJsonFullUserRoleDto :: DecodeJson FullUserRoleDto
derive instance genericFullUserRoleDto :: Generic FullUserRoleDto _
instance showFullUserRoleDto :: Show FullUserRoleDto where
  show = genericShow

data Role = Admin | Member

instance encodeJsonRole :: EncodeJson Role where
  encodeJson Admin = encodeJson "Admin"
  encodeJson Member = encodeJson "Member"

instance decodeJsonRole :: DecodeJson Role where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "Admin" -> Right Admin
      "Member" -> Right Member
      _ -> Left $ UnexpectedValue json

stringToRole :: String -> Maybe Role
stringToRole "Admin" = Just Admin
stringToRole "Member" = Just Member
stringToRole _ = Nothing

derive instance eqRole :: Eq Role
derive instance genericRole :: Generic Role _
instance showRole :: Show Role where
  show = genericShow
