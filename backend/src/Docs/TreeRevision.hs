{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Docs.TreeRevision
-- Description : A Revision of a Tree
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module contains definitions and related utility functions for revisions of the
-- document structure tree.
module Docs.TreeRevision
    ( TreeRevisionID (..)
    , TreeRevision (..)
    , TreeRevisionHeader (..)
    , TreeRevisionSelector (..)
    , TreeRevisionHistory (..)
    , TreeRevisionRef (..)
    , prettyPrintTreeRevisionRef
    , mapRoot
    , mapMRoot
    , replaceRoot
    , withTextRevisions
    , newTreeRevision
    , specificTreeRevision
    , latestTreeRevisionAsOf
    ) where

import Control.Monad (unless)
import Data.Functor ((<&>))
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
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( HasFormat (format)
    , NamedSchema (..)
    , OpenApiType (..)
    , Referenced (Inline)
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

import UserManagement.User (UserID)

import Data.Typeable (typeRep)
import Docs.Document (DocumentID (..))
import Docs.TextElement (TextElement, TextElementID)
import Docs.TextRevision (TextElementRevision, TextRevision)
import Docs.Tree (Node)
import qualified Docs.Tree as Tree
import Docs.UserRef (UserRef)
import Parse (parseFlexibleTime)

-- | References a @TreeRevision@
data TreeRevisionRef
    = TreeRevisionRef
        DocumentID
        TreeRevisionSelector
    deriving (Generic)

instance ToJSON TreeRevisionRef

instance FromJSON TreeRevisionRef

instance ToSchema TreeRevisionRef

-- | Optisch ansprechende Darstellung der Baum-Revisions-Referenz.
prettyPrintTreeRevisionRef :: TreeRevisionRef -> String
prettyPrintTreeRevisionRef (TreeRevisionRef treeElementRef selector) =
    show (unDocumentID treeElementRef) ++ prettyPrintSelector selector
  where
    prettyPrintSelector Latest = "latest"
    prettyPrintSelector (LatestAsOf ts) = show ts
    prettyPrintSelector (Specific revID) = show $ unTreeRevisionID revID

-- | Selector for a tree revision.
data TreeRevisionSelector
    = -- | select the latest revision
      Latest
    | -- | selects the latest revision at a given timestamp
      LatestAsOf UTCTime
    | -- | selects a specific revision by its id
      Specific TreeRevisionID

instance ToJSON TreeRevisionSelector where
    toJSON Latest = toJSON ("latest" :: Text)
    toJSON (LatestAsOf ts) = toJSON ts
    toJSON (Specific id_) = toJSON id_

instance FromJSON TreeRevisionSelector where
    parseJSON v = case v of
        Aeson.String "latest" -> pure Latest
        Aeson.String _ -> LatestAsOf <$> parseJSON v -- TODO: parseFlexibleTime?
        Aeson.Number n -> case toBoundedInteger n of
            Just i -> pure $ Specific $ TreeRevisionID i
            Nothing -> fail "Invalid number for Int64"
        _ -> fail "TreeRevisionSelector must be either a string \"latest\" or an integer"

instance ToSchema TreeRevisionSelector where
    declareNamedSchema _ = do
        intSchema <- declareSchemaRef (Proxy :: Proxy Int64)
        timestampSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
        let latestSchema =
                Inline $
                    mempty
                        & type_ ?~ OpenApiString
                        & enum_ ?~ ["latest"]
        return $
            NamedSchema (Just "TreeRevisionSelector") $
                mempty & oneOf ?~ [latestSchema, intSchema, timestampSchema]

instance ToParamSchema TreeRevisionSelector where
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

instance FromHttpApiData TreeRevisionSelector where
    parseUrlPiece txt
        | Text.toLower txt == "latest" = Right Latest
        | otherwise =
            case readMaybe (Text.unpack txt) of
                Just i -> Right $ Specific $ TreeRevisionID i
                Nothing ->
                    case parseFlexibleTime (Text.unpack txt) of
                        Just ts -> Right $ LatestAsOf ts
                        Nothing -> Left $ "Invalid TreeRevisionSelector: " <> txt

-- | Obtain id if selects a specific revision.
specificTreeRevision :: TreeRevisionSelector -> Maybe TreeRevisionID
specificTreeRevision (Specific id_) = Just id_
specificTreeRevision _ = Nothing

-- | Obtain timestamp if selects @LatestAsOf@.
latestTreeRevisionAsOf :: TreeRevisionSelector -> Maybe UTCTime
latestTreeRevisionAsOf (LatestAsOf ts) = Just ts
latestTreeRevisionAsOf _ = Nothing

-- | An ID for a tree revision.
newtype TreeRevisionID = TreeRevisionID
    { unTreeRevisionID :: Int64
    }
    deriving (Eq)

instance ToJSON TreeRevisionID where
    toJSON = toJSON . unTreeRevisionID

instance FromJSON TreeRevisionID where
    parseJSON = fmap TreeRevisionID . parseJSON

instance ToSchema TreeRevisionID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema TreeRevisionID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData TreeRevisionID where
    parseUrlPiece = (TreeRevisionID <$>) . parseUrlPiece

-- | Contains metadata about a tree revision.
data TreeRevisionHeader = TreeRevisionHeader
    { identifier :: TreeRevisionID
    , timestamp :: UTCTime
    , author :: UserRef
    }
    deriving (Generic)

instance ToJSON TreeRevisionHeader

instance FromJSON TreeRevisionHeader

instance ToSchema TreeRevisionHeader

-- | A tree revision.
data TreeRevision a
    = TreeRevision
        TreeRevisionHeader
        (Node a)

instance (ToJSON a) => ToJSON (TreeRevision a) where
    toJSON (TreeRevision header root) =
        Aeson.object ["header" .= header, "root" .= root]

instance (FromJSON a) => FromJSON (TreeRevision a) where
    parseJSON = Aeson.withObject "TreeRevision" $ \v ->
        TreeRevision
            <$> v .: "header"
            <*> v .: "root"

instance (ToSchema a) => ToSchema (TreeRevision a) where
    declareNamedSchema _ = do
        headerSchema <- declareSchemaRef (Proxy :: Proxy TreeRevisionHeader)
        rootSchema <- declareSchemaRef (Proxy :: Proxy (Node a))
        return
            $ NamedSchema
                (Just $ withTypeName "TreeRevision")
            $ mempty
                & type_ ?~ OpenApiObject
                & properties
                    .~ InsOrd.fromList
                        [("header", headerSchema), ("root", rootSchema)]
                & required .~ ["header", "root"]
      where
        withTypeName s = Text.pack $ s ++ " " ++ typeName
        typeName = show $ typeRep (Proxy :: Proxy a)

-- | Sequence of revisions for a document tree.
data TreeRevisionHistory
    = TreeRevisionHistory
        DocumentID
        [TreeRevisionHeader]

instance ToJSON TreeRevisionHistory where
    toJSON (TreeRevisionHistory docID history) =
        Aeson.object ["document" .= docID, "history" .= history]

instance FromJSON TreeRevisionHistory where
    parseJSON = Aeson.withObject "TreeRevisionHistory" $ \v ->
        TreeRevisionHistory
            <$> v .: "document"
            <*> v .: "history"

instance ToSchema TreeRevisionHistory where
    declareNamedSchema _ = do
        documentSchema <- declareSchemaRef (Proxy :: Proxy DocumentID)
        historySchema <- declareSchemaRef (Proxy :: Proxy [TreeRevisionHeader])
        return $
            NamedSchema (Just "TreeRevisionHistory") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("document", documentSchema)
                            , ("history", historySchema)
                            ]
                    & required .~ ["document", "history"]

mapRoot :: (Node a -> Node b) -> TreeRevision a -> TreeRevision b
mapRoot f (TreeRevision header root) = TreeRevision header $ f root

replaceRoot :: Node b -> TreeRevision a -> TreeRevision b
replaceRoot new = mapRoot $ const new

mapMRoot
    :: (Monad m)
    => (Node a -> m (Node b))
    -> TreeRevision a
    -> m (TreeRevision b)
mapMRoot f (TreeRevision header root) = f root <&> TreeRevision header

instance Functor TreeRevision where
    fmap f (TreeRevision header root) = TreeRevision header $ f <$> root

-- | Takes a tree revision and emplaces concrecte text revisions.
-- | The text revisions are obtained via the specified getter function.
withTextRevisions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text revision
    -> TreeRevision TextElement
    -- ^ document structre tree revision
    -> m (TreeRevision TextElementRevision)
    -- ^ document structre tree revision with concrete text revision
withTextRevisions getTextRevision = mapMRoot (Tree.withTextRevisions getTextRevision)

newTreeRevision
    :: (Monad m)
    => (DocumentID -> m (TextElementID -> Bool))
    -- ^ for a given document, checks if a text element belongs to this document
    -> (UserID -> DocumentID -> Node TextElementID -> m (TreeRevision TextElementID))
    -- ^ create a new tree revision
    -> UserID
    -- ^ authors user id
    -> DocumentID
    -- ^ which document the revision belongs to
    -> Node TextElementID
    -- ^ the root node of the revision
    -> m (TreeRevision TextElementID)
    -- ^ newly created revision
newTreeRevision
    isTextElementInDocument
    createRevision
    authorID
    docID
    rootNode = do
        isTextElementInDocument' <- isTextElementInDocument docID
        let allTextElementsBelongToDocument =
                all isTextElementInDocument' rootNode
        unless allTextElementsBelongToDocument $
            error "Not all referenced text elements belong to the document."
        createRevision authorID docID rootNode
