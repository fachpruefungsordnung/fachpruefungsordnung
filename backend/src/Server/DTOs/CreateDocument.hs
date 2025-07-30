{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.CreateDocument (CreateDocument (..)) where

import Data.Text (Text)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)

import UserManagement.Group (GroupID)

data CreateDocument = CreateDocument
    { groupID :: GroupID
    , title :: Text
    }
    deriving (Generic)

instance ToJSON CreateDocument

instance FromJSON CreateDocument

instance ToSchema CreateDocument
