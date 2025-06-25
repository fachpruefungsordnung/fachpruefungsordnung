{-# LANGUAGE DeriveGeneric #-}

module UserManagement.Group (GroupCreate (..), GroupID, GroupOverview (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Int (Int32)

-- | List of GroupIds and GroupName pairs
data GroupOverview = GroupOverview
    { groupID :: GroupID
    , groupName :: Text
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

type GroupID = Int32
