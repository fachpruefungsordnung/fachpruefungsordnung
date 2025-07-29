{-# LANGUAGE DeriveGeneric #-}

module Docs.UserRef (UserRef (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)

import GHC.Generics (Generic)

import UserManagement.User (UserID)

data UserRef = UserRef
    { identifier :: UserID
    , name :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON UserRef

instance FromJSON UserRef

instance ToSchema UserRef
