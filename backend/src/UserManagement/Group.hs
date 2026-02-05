{-# LANGUAGE DeriveGeneric #-}

module UserManagement.Group (Group (..), GroupCreate (..), GroupID, GroupOverview (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import qualified UserManagement.User as User

-- | Represents all information about a single group
data Group = Group
    { groupID :: GroupID
    , groupName :: Text
    , groupDescription :: Maybe Text
    , groupMembers :: [User.UserInfo]
    }
    deriving (Generic)

instance ToJSON Group
instance ToSchema Group

-- | List of GroupIds and GroupName pairs
data GroupOverview = GroupOverview
    { groupOverviewID :: GroupID
    , groupOverviewName :: Text
    , groupOverviewDescription :: Text
    }
    deriving (Generic)

instance ToSchema GroupOverview
instance ToJSON GroupOverview

data GroupCreate = GroupCreate
    { groupCreateName :: Text
    , groupCreateDescription :: Maybe Text
    }
    deriving (Eq, Generic)

instance FromJSON GroupCreate
instance ToSchema GroupCreate

type GroupID = Int64
