{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.CreateTextElement (CreateTextElement (..)) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Data.OpenApi (ToSchema)

import Docs.TextElement (TextElementKind)

newtype CreateTextElement
    = CreateTextElement
    { kind :: TextElementKind
    }
    deriving (Generic)

instance ToJSON CreateTextElement

instance FromJSON CreateTextElement

instance ToSchema CreateTextElement
