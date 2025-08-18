{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.Comments (Comments (..)) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Vector (Vector)
import Docs.Comment (Comment)

newtype Comments = Comments
    { comments :: Vector Comment
    }
    deriving (Generic)

instance ToJSON Comments

instance FromJSON Comments

instance ToSchema Comments
