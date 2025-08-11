module FPO.Dto.UserRoleDto where

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

newtype FullUserRoleDto = FullUserRoleDto
  { groupID :: Int
  , groupName :: String
  , role :: Role
  }

getGroupID :: FullUserRoleDto -> Int
getGroupID (FullUserRoleDto r) = r.groupID

getUserRole :: FullUserRoleDto -> Role
getUserRole (FullUserRoleDto r) = r.role

getGroupName :: FullUserRoleDto -> String
getGroupName (FullUserRoleDto r) = r.groupName

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
