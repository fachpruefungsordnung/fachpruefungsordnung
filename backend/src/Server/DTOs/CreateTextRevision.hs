{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.CreateTextRevision (CreateTextRevision (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)

import GHC.Generics (Generic)

import Docs.TextRevision (TextRevisionID)

data CreateTextRevision
    = CreateTextRevision
    { parent :: Maybe TextRevisionID
    , content :: Text
    }
    deriving (Generic)

instance ToJSON CreateTextRevision

instance FromJSON CreateTextRevision

instance ToSchema CreateTextRevision
