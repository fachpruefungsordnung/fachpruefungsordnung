{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.DTOs.CreateTextRevision (CreateTextRevision (..)) where

import Data.Aeson (FromJSON (parseJSON), ToJSON, (.!=), (.:), (.:?))
import Data.OpenApi (ToSchema)
import Data.Text (Text)

import GHC.Generics (Generic)

import Data.Aeson.Types (withObject)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Docs.Comment (CommentAnchor)
import Docs.TextRevision (TextRevisionID)

data CreateTextRevision
    = CreateTextRevision
    { parent :: Maybe TextRevisionID
    , content :: Text
    , commentAnchors :: Vector CommentAnchor
    -- | Whether this is an auto-save operation. Auto-saves create drafts on conflicts,
    -- while manual saves return conflict errors. This distinguishes the conflict resolution strategy.
    , isAutoSave :: Bool
    }
    deriving (Generic)

instance ToJSON CreateTextRevision

instance FromJSON CreateTextRevision where
    parseJSON = withObject "CreateTextRevision" $ \o ->
        CreateTextRevision
            <$> o .:? "parent"
            <*> o .: "content"
            <*> (o .:? "commentAnchors" .!= Vector.empty)
            <*> (o .:? "isAutoSave" .!= False)

instance ToSchema CreateTextRevision
