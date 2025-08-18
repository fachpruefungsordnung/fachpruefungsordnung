{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.CreateComment (CreateComment (..)) where

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)

newtype CreateComment = CreateComment
    { text :: Text
    }
    deriving (Generic)

instance ToJSON CreateComment

instance FromJSON CreateComment

instance ToSchema CreateComment
