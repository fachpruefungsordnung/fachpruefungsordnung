{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.Documents (Documents (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Vector (Vector)

import GHC.Generics (Generic)

import Docs.Document (Document)

newtype Documents = Documents
    { documents :: Vector Document
    }
    deriving (Generic)

instance ToJSON Documents

instance FromJSON Documents

instance ToSchema Documents
