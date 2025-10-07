{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Docs.DocumentHistory
-- Description : Datatypes Representing a Documents History
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module contains data type definitions representing the history
-- (meaning chronological overview of revisions) of a @Document@
module Docs.DocumentHistory
    ( DocumentHistory (..)
    , DocumentHistoryItem (..)
    ) where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)

import GHC.Generics (Generic)

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , Referenced (Inline)
    , Schema
    , ToSchema (..)
    , declareSchemaRef
    , enum_
    , oneOf
    , properties
    , required
    , type_
    )

import Docs.Document (DocumentID)
import Docs.TextElement (TextElementID)
import Docs.TextRevision (TextRevisionHeader)
import Docs.TreeRevision (TreeRevisionHeader)

-- | An item in the @Document@s history.
-- This represents a generic revision.
-- This revision can either be a @Tree@ or a @TextElement@ revision.
data DocumentHistoryItem
    = Tree TreeRevisionHeader
    | Text TextElementID TextRevisionHeader

instance ToJSON DocumentHistoryItem where
    toJSON (Tree header) =
        Aeson.object
            [ "type" .= ("tree" :: Text)
            , "header" .= header
            ]
    toJSON (Text textID header) =
        Aeson.object
            [ "type" .= ("text" :: Text)
            , "text" .= textID
            , "header" .= header
            ]

instance FromJSON DocumentHistoryItem where
    parseJSON = Aeson.withObject "DocumentHistoryItem" $ \obj -> do
        ty <- obj .: "type" :: Parser Text
        case ty of
            "tree" -> Tree <$> obj .: "header"
            "text" -> Text <$> obj .: "text" <*> obj .: "header"
            _ -> fail $ "Unknown DocumentHistoryItem type: " ++ show ty

instance ToSchema DocumentHistoryItem where
    declareNamedSchema _ = do
        treeHeaderSchema <- declareSchemaRef (Proxy :: Proxy TreeRevisionHeader)
        textHeaderSchema <- declareSchemaRef (Proxy :: Proxy TextRevisionHeader)
        textSchema <- declareSchemaRef (Proxy :: Proxy TextElementID)
        let treeVariant =
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("type", Inline $ schemaConstText "tree")
                            , ("header", treeHeaderSchema)
                            ]
                    & required .~ ["type", "header"]
        let textVariant =
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("type", Inline $ schemaConstText "text")
                            , ("text", textSchema)
                            , ("header", textHeaderSchema)
                            ]
                    & required .~ ["type", "text", "header"]
        return $
            NamedSchema (Just "DocumentHistoryItem") $
                mempty
                    & type_ ?~ OpenApiObject
                    & oneOf
                        ?~ [ Inline treeVariant
                           , Inline textVariant
                           ]
      where
        schemaConstText :: Text -> Schema
        schemaConstText val =
            mempty
                & type_ ?~ OpenApiString
                & enum_ ?~ [toJSON val]

-- | Chronological overview of a @Document@s history of revisions.
data DocumentHistory = DocumentHistory
    { document :: DocumentID
    , history :: [DocumentHistoryItem]
    }
    deriving (Generic)

instance ToJSON DocumentHistory

instance FromJSON DocumentHistory

instance ToSchema DocumentHistory
