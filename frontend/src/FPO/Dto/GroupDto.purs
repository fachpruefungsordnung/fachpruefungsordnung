-- | All DTOs and data representations related to groups.
module FPO.Dto.GroupDto where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array (find)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import FPO.Dto.UserDto (Role, UserID)

type GroupID = Int

-- | Represents a group overview entity, as returned by the `GET /groups` endpoint.
newtype GroupOverview = GroupOverview
  { groupOverviewName :: String, groupOverviewID :: GroupID }

getGroupOverviewID :: GroupOverview -> Int
getGroupOverviewID (GroupOverview g) = g.groupOverviewID

getGroupOverviewName :: GroupOverview -> String
getGroupOverviewName (GroupOverview g) = g.groupOverviewName

derive instance newtypeGroupOverview :: Newtype GroupOverview _
derive newtype instance encodeJsonGroupOverview :: EncodeJson GroupOverview
derive newtype instance decodeJsonGroupOverview :: DecodeJson GroupOverview

-- | A group creation request DTO, as sent to the `POST /groups` endpoint.
newtype GroupCreate = GroupCreate
  { groupCreateName :: String, groupCreateDescription :: String }

derive instance newtypeGroupCreate :: Newtype GroupCreate _
derive newtype instance encodeJsonGroupCreate :: EncodeJson GroupCreate
derive newtype instance decodeJsonGroupCreate :: DecodeJson GroupCreate

-- | Represents a group entity, as returned by the `GET /groups/{groupID}` endpoint.
newtype GroupDto = GroupDto
  { groupDescription :: String
  , groupID :: GroupID
  , groupMembers :: Array GroupMemberDto
  , groupName :: String
  }

getGroupName :: GroupDto -> String
getGroupName (GroupDto g) = g.groupName

getGroupMembers :: GroupDto -> Array GroupMemberDto
getGroupMembers (GroupDto g) = g.groupMembers

lookupUser :: GroupDto -> UserID -> Maybe GroupMemberDto
lookupUser (GroupDto g) userID =
  find (\member -> getUserInfoID member == userID) g.groupMembers

derive instance newtypeGroupDto :: Newtype GroupDto _
derive newtype instance encodeJsonGroupDto :: EncodeJson GroupDto
derive newtype instance decodeJsonGroupDto :: DecodeJson GroupDto

-- | Represents a group member entity, as returned by the `GET /groups/{groupID}` endpoint.
newtype GroupMemberDto = GroupMemberDto
  { userInfoEmail :: String
  , userInfoID :: UserID
  , userInfoName :: String
  , userInfoRole :: Role
  }

getUserInfoName :: GroupMemberDto -> String
getUserInfoName (GroupMemberDto m) = m.userInfoName

getUserInfoRole :: GroupMemberDto -> Role
getUserInfoRole (GroupMemberDto m) = m.userInfoRole

getUserInfoID :: GroupMemberDto -> UserID
getUserInfoID (GroupMemberDto m) = m.userInfoID

derive instance newtypeGroupMemberDto :: Newtype GroupMemberDto _
derive newtype instance encodeJsonGroupMemberDto :: EncodeJson GroupMemberDto
derive newtype instance decodeJsonGroupMemberDto :: DecodeJson GroupMemberDto
