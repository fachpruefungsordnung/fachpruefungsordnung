-- | All DTOs and data representations related to groups.
module FPO.Dto.GroupDto
  ( GroupID
  , GroupOverview(..)
  , getGroupOverviewID
  , getGroupOverviewName
  , GroupCreate(..)
  , GroupDto(..)
  , getGroupName
  , getGroupDescription
  , getGroupMembers
  , lookupUser
  , isUserInGroup
  , GroupMemberDto(..)
  , getUserInfoName
  , getUserInfoRole
  , getUserInfoID
  , GroupPatch(..)
  , class ToGroupOverview
  , toGroupOverview
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Array (find)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, isJust)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import FPO.Dto.UserDto (UserID)
import FPO.Dto.UserRoleDto as UR

type GroupID = Int

-- | Represents a group overview entity, as returned by the `GET /groups` endpoint.
newtype GroupOverview = GroupOverview
  { groupOverviewName :: String
  , groupOverviewID :: GroupID
  , groupOverviewDescription :: String
  }

getGroupOverviewID :: GroupOverview -> Int
getGroupOverviewID (GroupOverview g) = g.groupOverviewID

getGroupOverviewName :: GroupOverview -> String
getGroupOverviewName (GroupOverview g) = g.groupOverviewName

derive instance newtypeGroupOverview :: Newtype GroupOverview _
derive newtype instance encodeJsonGroupOverview :: EncodeJson GroupOverview
derive newtype instance decodeJsonGroupOverview :: DecodeJson GroupOverview
derive instance genericGroupOverview :: Generic GroupOverview _
instance showGroupOverview :: Show GroupOverview where
  show = genericShow

-- | A group creation request DTO, as sent to the `POST /groups` endpoint.
-- | The `groupCreateUsers` field is an optional array of user UUIDs to add
-- | as members when the group is created.
newtype GroupCreate = GroupCreate
  { groupCreateName :: String
  , groupCreateDescription :: String
  , groupCreateUsers :: Array UserID
  }

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

getGroupDescription :: GroupDto -> String
getGroupDescription (GroupDto g) = g.groupDescription

getGroupMembers :: GroupDto -> Array GroupMemberDto
getGroupMembers (GroupDto g) = g.groupMembers

lookupUser :: GroupDto -> UserID -> Maybe GroupMemberDto
lookupUser (GroupDto g) userID =
  find (\member -> getUserInfoID member == userID) g.groupMembers

isUserInGroup :: GroupDto -> UserID -> Boolean
isUserInGroup g = isJust <<< lookupUser g

derive instance newtypeGroupDto :: Newtype GroupDto _
derive newtype instance encodeJsonGroupDto :: EncodeJson GroupDto
derive newtype instance decodeJsonGroupDto :: DecodeJson GroupDto
derive instance genericGroupDto :: Generic GroupDto _
instance showGroupDto :: Show GroupDto where
  show = genericShow

-- | Represents a group member entity, as returned by the `GET /groups/{groupID}` endpoint.
newtype GroupMemberDto = GroupMemberDto
  { userInfoEmail :: String
  , userInfoID :: UserID
  , userInfoName :: String
  , userInfoRole :: UR.Role
  }

getUserInfoName :: GroupMemberDto -> String
getUserInfoName (GroupMemberDto m) = m.userInfoName

getUserInfoRole :: GroupMemberDto -> UR.Role
getUserInfoRole (GroupMemberDto m) = m.userInfoRole

getUserInfoID :: GroupMemberDto -> UserID
getUserInfoID (GroupMemberDto m) = m.userInfoID

derive instance newtypeGroupMemberDto :: Newtype GroupMemberDto _
derive newtype instance encodeJsonGroupMemberDto :: EncodeJson GroupMemberDto
derive newtype instance decodeJsonGroupMemberDto :: DecodeJson GroupMemberDto
derive instance genericGroupMemberDto :: Generic GroupMemberDto _
instance showGroupMemberDto :: Show GroupMemberDto where
  show = genericShow

class ToGroupOverview a where
  toGroupOverview :: a -> GroupOverview

instance toGroupOverviewGroupDto :: ToGroupOverview GroupDto where
  toGroupOverview (GroupDto g) = GroupOverview
    { groupOverviewName: g.groupName
    , groupOverviewID: g.groupID
    , groupOverviewDescription: g.groupDescription
    }

instance toGroupOverviewGroupOverview :: ToGroupOverview UR.FullUserRoleDto where
  toGroupOverview r = GroupOverview
    { groupOverviewName: UR.getGroupName r
    , groupOverviewID: UR.getGroupID r
    , groupOverviewDescription: ""
    }

-- | A group patch request DTO, as sent to the `PATCH /groups/{groupID}` endpoint.
-- | Both fields are optional, but at least one must be provided.
-- | `patchDescription` is a Maybe (Maybe String) to allow:
-- |   - Nothing: don't change description
-- |   - Just Nothing: set description to null
-- |   - Just (Just "...") : set description to new value
newtype GroupPatch = GroupPatch
  { patchName :: Maybe String
  , patchDescription :: Maybe (Maybe String)
  }

derive instance newtypeGroupPatch :: Newtype GroupPatch _
derive newtype instance encodeJsonGroupPatch :: EncodeJson GroupPatch
derive newtype instance decodeJsonGroupPatch :: DecodeJson GroupPatch
