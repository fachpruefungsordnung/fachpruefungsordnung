{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.CreateReply (CreateReply (..)) where

import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)

newtype CreateReply = CreateReply
    { text :: Text
    }
    deriving (Generic)

instance ToJSON CreateReply

instance FromJSON CreateReply

instance ToSchema CreateReply
