-- | All DTOs and data representations related to groups.
module FPO.Dto.GroupDto where

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Newtype (class Newtype)

-- | Represents a group overview entity, as returned by the `GET /groups` endpoint.
newtype GroupOverview = GroupOverview
  { groupOverviewName :: String, groupOverviewID :: Int }

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
