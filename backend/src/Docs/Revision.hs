{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs.Revision
    ( RevisionID (..)
    , TextOrTree (..)
    , RevisionKey (..)
    , RevisionRef (..)
    , RevisionSelector (..)
    , selectorFromTextRevision
    , refFromTextRevision
    , specificRevision
    , latestRevisionAsOf
    , textRevisionRefFor
    , treeRevisionRefFor
    , prettyPrintRevisionRef
    ) where

import Control.Lens ((?~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.OpenApi
    ( HasEnum (enum_)
    , HasExclusiveMinimum (exclusiveMinimum)
    , HasFormat (format)
    , HasMinimum (minimum_)
    , HasOneOf (oneOf)
    , HasType (type_)
    , NamedSchema (NamedSchema)
    , OpenApiType (OpenApiInteger, OpenApiString)
    , Referenced (Inline)
    , ToSchema
    , declareSchemaRef
    )
import Data.OpenApi.ParamSchema
import Data.OpenApi.Schema (declareNamedSchema)
import Data.Proxy (Proxy (Proxy))
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Docs.Document (DocumentID (unDocumentID))
import Docs.TextElement (TextElementRef (TextElementRef))
import Docs.TextRevision
    ( TextRevisionRef (TextRevisionRef)
    , TextRevisionSelector
    )
import qualified Docs.TextRevision as TextRevision
import Docs.TreeRevision (TreeRevisionRef (TreeRevisionRef))
import qualified Docs.TreeRevision as TreeRevision
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Parse (parseFlexibleTime)
import Servant (FromHttpApiData (parseUrlPiece))
import Text.Read (readMaybe)

newtype RevisionID = RevisionID
    { unRevisionID :: Int64
    }
    deriving (Eq, Ord, Show)

instance ToJSON RevisionID where
    toJSON = toJSON . unRevisionID

instance FromJSON RevisionID where
    parseJSON = fmap RevisionID . parseJSON

instance ToSchema RevisionID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema RevisionID where
    toParamSchema _ =
        mempty
            & type_
                ?~ OpenApiInteger
            & minimum_
                ?~ 0
            & exclusiveMinimum
                ?~ False

instance FromHttpApiData RevisionID where
    parseUrlPiece = (RevisionID <$>) . parseUrlPiece

-- | Selector for a revision
data RevisionSelector
    = Latest
    | LatestAsOf UTCTime
    | Specific RevisionID
    deriving (Show)

selectorFromTextRevision :: TextRevisionSelector -> RevisionSelector
selectorFromTextRevision TextRevision.Latest = Latest
selectorFromTextRevision (TextRevision.LatestAsOf time) = LatestAsOf time
selectorFromTextRevision (TextRevision.Specific id_) =
    Specific $ RevisionID $ TextRevision.unTextRevisionID id_

instance ToJSON RevisionSelector where
    toJSON Latest = toJSON ("latest" :: Text)
    toJSON (LatestAsOf ts) = toJSON ts
    toJSON (Specific id_) = toJSON id_

instance FromJSON RevisionSelector where
    parseJSON v = case v of
        Aeson.String "latest" -> pure Latest
        Aeson.String _ -> LatestAsOf <$> parseJSON v -- TODO: parseFlexibleTime?
        Aeson.Number n -> case toBoundedInteger n of
            Just i -> pure $ Specific $ RevisionID i
            Nothing -> fail "Invalid number for Int64"
        _ -> fail "RevisionSelector must be either a string \"latest\" or an integer"

instance ToSchema RevisionSelector where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int64)
        timestampSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
        let latestSchema =
                Inline $
                    mempty
                        & type_ ?~ OpenApiString
                        & enum_ ?~ ["latest"]
        return $
            NamedSchema (Just "RevisionSelector") $
                mempty & oneOf ?~ [latestSchema, intSchema, timestampSchema]

instance ToParamSchema RevisionSelector where
    toParamSchema _ =
        mempty
            & oneOf
                ?~ [ Inline $
                        mempty
                            & type_ ?~ OpenApiString
                            & enum_ ?~ ["latest"]
                   , Inline $
                        mempty
                            & type_ ?~ OpenApiString
                            & format ?~ "date-time"
                   , Inline $
                        mempty
                            & type_ ?~ OpenApiInteger
                            & minimum_ ?~ 0
                            & exclusiveMinimum ?~ False
                   ]

instance FromHttpApiData RevisionSelector where
    parseUrlPiece txt
        | Text.toLower txt == "latest" = Right Latest
        | otherwise =
            case readMaybe (Text.unpack txt) of
                Just i -> Right $ Specific $ RevisionID i
                Nothing ->
                    case parseFlexibleTime (Text.unpack txt) of
                        Just ts -> Right $ LatestAsOf ts
                        Nothing -> Left $ "Invalid RevisionSelector: " <> txt

specificRevision :: RevisionSelector -> Maybe RevisionID
specificRevision (Specific id_) = Just id_
specificRevision _ = Nothing

latestRevisionAsOf :: RevisionSelector -> Maybe UTCTime
latestRevisionAsOf (LatestAsOf ts) = Just ts
latestRevisionAsOf _ = Nothing

data RevisionRef
    = RevisionRef DocumentID RevisionSelector
    deriving (Generic)

refFromTextRevision :: TextRevisionRef -> RevisionRef
refFromTextRevision (TextRevisionRef (TextElementRef docID _) selector) =
    RevisionRef docID $ selectorFromTextRevision selector

instance ToJSON RevisionRef

instance FromJSON RevisionRef

instance ToSchema RevisionRef

prettyPrintRevisionRef :: RevisionRef -> String
prettyPrintRevisionRef (RevisionRef docID revSelector) =
    show (unDocumentID docID) ++ ".rev." ++ show revSelector

data TextOrTree
    = Text TextRevisionRef
    | Tree TreeRevisionRef
    deriving (Generic)

instance ToJSON TextOrTree

instance FromJSON TextOrTree

instance ToSchema TextOrTree

data RevisionKey = RevisionKey
    { timestamp :: UTCTime
    , revision :: TextOrTree
    }
    deriving (Generic)

instance ToJSON RevisionKey

instance FromJSON RevisionKey

instance ToSchema RevisionKey

textRevisionRefFor :: TextElementRef -> RevisionKey -> TextRevisionRef
textRevisionRefFor
    forTextRef
    ( RevisionKey
            { timestamp = ts
            , revision = Text ref@(TextRevisionRef textRef _)
            }
        )
        | textRef == forTextRef = ref
        | otherwise = TextRevisionRef forTextRef $ TextRevision.LatestAsOf ts
textRevisionRefFor forTextRef (RevisionKey {timestamp = ts}) =
    TextRevisionRef forTextRef $ TextRevision.LatestAsOf ts

treeRevisionRefFor :: DocumentID -> RevisionKey -> TreeRevisionRef
treeRevisionRefFor
    forDocID
    ( RevisionKey
            { timestamp = ts
            , revision = Tree ref@(TreeRevisionRef docID _)
            }
        )
        | docID == forDocID = ref
        | otherwise = TreeRevisionRef forDocID $ TreeRevision.LatestAsOf ts
treeRevisionRefFor forDocID (RevisionKey {timestamp = ts}) =
    TreeRevisionRef forDocID $ TreeRevision.LatestAsOf ts
