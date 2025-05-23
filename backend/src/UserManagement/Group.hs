{-# LANGUAGE DeriveGeneric #-}

module UserManagement.Group (Group (..), GroupID) where

import Data.Aeson (FromJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Int (Int32)

data Group = Group
    { groupName :: Text
    , groupDescription :: Maybe Text
    }
    deriving (Eq, Generic)

instance FromJSON Group
instance ToSchema Group

type GroupID = Int32