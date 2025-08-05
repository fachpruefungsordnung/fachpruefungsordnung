{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.Documents (Documents (..), DocumentsQuery (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Vector (Vector)

import GHC.Generics (Generic)

import Docs.Document (Document)
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

data Documents = Documents
    { documents :: Vector Document
    , query :: DocumentsQuery
    }
    deriving (Generic)

instance ToJSON Documents

instance FromJSON Documents

instance ToSchema Documents

data DocumentsQuery = DocumentsQuery
    { user :: Maybe UserID
    , group :: Maybe GroupID
    }
    deriving (Generic)

instance ToJSON DocumentsQuery

instance FromJSON DocumentsQuery

instance ToSchema DocumentsQuery
