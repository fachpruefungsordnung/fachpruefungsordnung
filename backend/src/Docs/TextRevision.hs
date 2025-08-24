{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs.TextRevision
    ( TextRevisionID (..)
    , TextRevisionSelector (..)
    , TextRevision (..)
    , TextRevisionHeader (..)
    , TextElementRevision (..)
    , ConflictStatus (..)
    , TextRevisionHistory (..)
    , NewTextRevision (..)
    , TextRevisionRef (..)
    , prettyPrintTextRevisionRef
    , textRevisionRef
    , specificTextRevision
    ) where

import Data.Proxy (Proxy (Proxy))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)

import Text.Read (readMaybe)

import GHC.Generics (Generic)
import GHC.Int (Int64)

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , Referenced (Inline)
    , Schema (..)
    , ToParamSchema (..)
    , ToSchema (..)
    , declareSchemaRef
    , enum_
    , exclusiveMinimum
    , minimum_
    , oneOf
    , properties
    , required
    , type_
    )
import Web.HttpApiData (FromHttpApiData (..))

import Data.Vector (Vector)
import Docs.Comment (CommentAnchor)
import Docs.Document (DocumentID)
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementRef (..)
    , prettyPrintTextElementRef
    )
import Docs.UserRef (UserRef)

data TextRevisionRef
    = TextRevisionRef
        TextElementRef
        TextRevisionSelector
    deriving (Generic)

instance ToJSON TextRevisionRef

instance FromJSON TextRevisionRef

instance ToSchema TextRevisionRef

prettyPrintTextRevisionRef :: TextRevisionRef -> String
prettyPrintTextRevisionRef (TextRevisionRef textElementRef selector) =
    prettyPrintTextElementRef textElementRef ++ prettyPrintSelector selector
  where
    prettyPrintSelector Latest = "latest"
    prettyPrintSelector (Specific revID) = show $ unTextRevisionID revID

textRevisionRef
    :: DocumentID
    -> TextElementID
    -> TextRevisionSelector
    -> TextRevisionRef
textRevisionRef = (TextRevisionRef .) . TextElementRef

-- | ID for a text revision
newtype TextRevisionID = TextRevisionID
    { unTextRevisionID :: Int64
    }
    deriving (Eq, Ord, Show)

instance ToJSON TextRevisionID where
    toJSON = toJSON . unTextRevisionID

instance FromJSON TextRevisionID where
    parseJSON = fmap TextRevisionID . parseJSON

instance ToSchema TextRevisionID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema TextRevisionID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData TextRevisionID where
    parseUrlPiece = (TextRevisionID <$>) . parseUrlPiece

-- | Selector for a text revision
data TextRevisionSelector
    = Latest
    | Specific TextRevisionID

instance ToJSON TextRevisionSelector where
    toJSON Latest = toJSON ("latest" :: Text)
    toJSON (Specific id_) = toJSON id_

instance FromJSON TextRevisionSelector where
    parseJSON v = case v of
        Aeson.String t ->
            if t == "latest"
                then pure Latest
                else fail $ "Invalid string for TextRevisionSelector: " ++ Text.unpack t
        Aeson.Number n -> case toBoundedInteger n of
            Just i -> pure $ Specific $ TextRevisionID i
            Nothing -> fail "Invalid number for Int64"
        _ -> fail "TextRevisionSelector must be either a string \"latest\" or an integer"

instance ToSchema TextRevisionSelector where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int64)
        let latestSchema =
                Inline $
                    mempty
                        & type_ ?~ OpenApiString
                        & enum_ ?~ ["latest"]
        return $
            NamedSchema (Just "TextRevisionSelector") $
                mempty & oneOf ?~ [latestSchema, intSchema]

instance ToParamSchema TextRevisionSelector where
    toParamSchema _ =
        mempty
            & oneOf
                ?~ [ Inline $
                        mempty
                            & type_ ?~ OpenApiString
                            & enum_ ?~ ["latest"]
                   , Inline $
                        mempty
                            & type_ ?~ OpenApiInteger
                            & minimum_ ?~ 0
                            & exclusiveMinimum ?~ False
                   ]

instance FromHttpApiData TextRevisionSelector where
    parseUrlPiece txt
        | Text.toLower txt == "latest" = Right Latest
        | otherwise = case readMaybe (Text.unpack txt) of
            Just i -> Right $ Specific $ TextRevisionID i
            Nothing -> Left $ "Invalid TextRevisionSelector: " <> txt

specificTextRevision :: TextRevisionSelector -> Maybe TextRevisionID
specificTextRevision Latest = Nothing
specificTextRevision (Specific id_) = Just id_

-- | Header of a text revision.
--   Contains metadata for a text revision.
data TextRevisionHeader = TextRevisionHeader
    { identifier :: TextRevisionID
    , timestamp :: UTCTime
    , author :: UserRef
    }
    deriving (Show, Generic)

instance ToJSON TextRevisionHeader

instance FromJSON TextRevisionHeader

instance ToSchema TextRevisionHeader

-- | A text revision.
data TextRevision
    = TextRevision
    { header :: TextRevisionHeader
    , content :: Text
    , commentAnchors :: Vector CommentAnchor
    }
    deriving (Generic)

instance ToJSON TextRevision

instance FromJSON TextRevision

instance ToSchema TextRevision

-- | A text revision with the text element it belongs to.
data TextElementRevision
    = TextElementRevision
    { textElement :: TextElement
    , revision :: Maybe TextRevision
    }
    deriving (Generic)

instance ToJSON TextElementRevision

instance FromJSON TextElementRevision

instance ToSchema TextElementRevision

-- | A sequence of revisions for a text element
data TextRevisionHistory
    = TextRevisionHistory
        TextElementRef
        [TextRevisionHeader]

instance ToJSON TextRevisionHistory where
    toJSON (TextRevisionHistory elementRef history) =
        Aeson.object ["textElement" .= elementRef, "history" .= history]

instance FromJSON TextRevisionHistory where
    parseJSON = Aeson.withObject "TextRevisionHistory" $ \v ->
        TextRevisionHistory
            <$> v .: "textElement"
            <*> v .: "history"

instance ToSchema TextRevisionHistory where
    declareNamedSchema _ = do
        textElementSchema <- declareSchemaRef (Proxy :: Proxy TextElementRef)
        historySchema <- declareSchemaRef (Proxy :: Proxy [TextRevisionHeader])
        return $
            NamedSchema (Just "TextRevisionHistory") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("textElement", textElementSchema)
                            , ("history", historySchema)
                            ]
                    & required .~ ["textElement", "history"]

-- | Information required to create a new text revision.
data NewTextRevision = NewTextRevision
    { newTextRevisionElement :: TextElementRef
    , newTextRevisionParent :: Maybe TextRevisionID
    , newTextRevisionContent :: Text
    , newTextRevisionCommentAnchors :: Vector CommentAnchor
    }

-- | A conflict with another text revision.
data ConflictStatus
    = Conflict TextRevisionID -- todo: maybe not id but whole TextRevision?
    | NoConflict TextRevision

instance ToJSON ConflictStatus where
    toJSON (Conflict conflictWith) =
        Aeson.object ["type" .= ("conflict" :: Text), "with" .= conflictWith]
    toJSON (NoConflict newRevision) =
        Aeson.object ["type" .= ("noConflict" :: Text), "newRevision" .= newRevision]

instance FromJSON ConflictStatus where
    parseJSON = Aeson.withObject "ConflictStatus" $ \obj -> do
        ty <- obj .: "type" :: Parser Text
        case ty of
            "conflict" -> Conflict <$> obj .: "with"
            "noConflict" -> NoConflict <$> obj .: "newRevision"
            _ -> fail $ "Unknown ConflictStatus type: " ++ show ty

instance ToSchema ConflictStatus where
    declareNamedSchema _ = do
        textRevIdSchema <- declareSchemaRef (Proxy :: Proxy TextRevisionID)
        textRevSchema <- declareSchemaRef (Proxy :: Proxy TextRevision)

        return $
            NamedSchema (Just "ConflictStatus") $
                mempty
                    & type_ ?~ OpenApiObject
                    & oneOf
                        ?~ [ Inline $
                                mempty
                                    & type_ ?~ OpenApiObject
                                    & properties
                                        .~ InsOrd.fromList
                                            [ ("type", Inline $ schemaConstText "conflict")
                                            , ("with", textRevIdSchema)
                                            ]
                                    & required .~ ["type", "with"]
                           , Inline $
                                mempty
                                    & type_ ?~ OpenApiObject
                                    & properties
                                        .~ InsOrd.fromList
                                            [ ("type", Inline $ schemaConstText "noConflict")
                                            , ("newRevision", textRevSchema)
                                            ]
                                    & required .~ ["type", "newRevision"]
                           ]
      where
        schemaConstText :: Text -> Schema
        schemaConstText val =
            mempty
                & type_ ?~ OpenApiString
                & enum_ ?~ [toJSON val]
