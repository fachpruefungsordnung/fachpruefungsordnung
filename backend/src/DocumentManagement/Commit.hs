{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DocumentManagement.Commit
    ( CreateCommit (..)
    , ExistingCommit (..)
    , CommitHeader (..)
    , CommitInfo (..)
    , CommitBody (..)
    , CommitID (..)
    , CommitRef
    , CommitRel (..)
    , CommitNode (..)
    , commitRefID
    , commitMapRoot
    )
where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , ToParamSchema (..)
    , ToSchema (..)
    , declareSchemaRef
    , exclusiveMinimum
    , minimum_
    , properties
    , required
    , type_
    )
import Data.Proxy
import Data.Text
import Data.Time (LocalTime)
import Data.UUID
import DocumentManagement.Hash
import DocumentManagement.Tree
import GHC.Generics
import GHC.Int
import Web.HttpApiData (FromHttpApiData (..))

-- | represents the id of a commit
newtype CommitID = CommitID
    { unCommitID :: Int32
    }
    deriving (Show, Generic, Eq, Ord)

instance ToJSON CommitID where
    toJSON = toJSON . unCommitID

instance FromJSON CommitID where
    parseJSON = fmap CommitID . parseJSON

instance ToSchema CommitID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int32)

instance ToParamSchema CommitID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData CommitID where
    parseUrlPiece = (CommitID <$>) . parseUrlPiece

-- | represents a reference to a commit or the commit itself
type CommitRef = Ref CommitID ExistingCommit

-- | extracts the 'CommitID' from a 'CommitRef'
commitRefID :: CommitRef -> CommitID
commitRefID (Ref ref) = ref
commitRefID (Value (ExistingCommit header _)) = commitHeaderID header

-- | contains all information needed to create a new commit
data CreateCommit
    = CreateCommit
    { createCommitInfo :: CommitInfo
    , createCommitRoot :: TreeRef NodeWithMaybeRef
    }
    deriving (Show)

instance ToJSON CreateCommit where
    toJSON (CreateCommit info root) =
        Aeson.object ["info" .= info, "root" .= root]

instance FromJSON CreateCommit where
    parseJSON = Aeson.withObject "CreateCommit" $ \v ->
        CreateCommit
            <$> v .: "info"
            <*> v .: "root"

instance ToSchema CreateCommit where
    declareNamedSchema _ = do
        infoSchema <- declareSchemaRef (Proxy :: Proxy CommitInfo)
        rootSchema <- declareSchemaRef (Proxy :: Proxy (TreeRef NodeWithMaybeRef))
        return $
            NamedSchema (Just "CreateCommit") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("info", infoSchema)
                            , ("root", rootSchema)
                            ]
                    & required .~ ["info", "root"]

-- | represents a commit guaranteed to exist in the database
data ExistingCommit
    = ExistingCommit
    { existingCommitHeader :: CommitHeader
    , existingCommitBody :: CommitBody
    }
    deriving (Show)

instance ToJSON ExistingCommit where
    toJSON (ExistingCommit header body) =
        Aeson.object ["header" .= header, "body" .= body]

instance FromJSON ExistingCommit where
    parseJSON = Aeson.withObject "ExistingCommit" $ \v ->
        ExistingCommit
            <$> v .: "header"
            <*> v .: "body"

instance ToSchema ExistingCommit where
    declareNamedSchema _ = do
        headerSchema <- declareSchemaRef (Proxy :: Proxy CommitHeader)
        bodySchema <- declareSchemaRef (Proxy :: Proxy CommitBody)
        return $
            NamedSchema (Just "ExistingCommit") $
                mempty
                    & type_
                        ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("header", headerSchema)
                            , ("body", bodySchema)
                            ]
                    & required
                        .~ ["header", "body"]

-- | maps the root node version of a commit by applying it to a given function
commitMapRoot
    :: (TreeRef (Hashed NodeWithRef) -> TreeRef (Hashed NodeWithRef))
    -> ExistingCommit
    -> ExistingCommit
commitMapRoot f (ExistingCommit header (CommitBody info root base)) =
    ExistingCommit header (CommitBody info (f root) base)

-- | contains the content of an existing commit
data CommitBody
    = CommitBody
        CommitInfo
        (TreeRef (Hashed NodeWithRef))
        (Maybe CommitID)
    deriving (Show)

instance ToJSON CommitBody where
    toJSON (CommitBody info root base) =
        Aeson.object ["info" .= info, "root" .= root, "base" .= base]

instance FromJSON CommitBody where
    parseJSON = Aeson.withObject "CommitBody" $ \v ->
        CommitBody
            <$> v .: "info"
            <*> v .: "root"
            <*> v .: "base"

instance ToSchema CommitBody where
    declareNamedSchema _ = do
        infoSchema <- declareSchemaRef (Proxy :: Proxy CommitInfo)
        rootSchema <- declareSchemaRef (Proxy :: Proxy (TreeRef (Hashed NodeWithRef)))
        return $
            NamedSchema (Just "CommitBody") $
                mempty
                    & type_
                        ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("info", infoSchema)
                            , ("root", rootSchema)
                            ]
                    & required
                        .~ ["info", "root"]

-- | additional metadata for existing commits
data CommitHeader = CommitHeader
    { commitHeaderID :: CommitID
    , commitHeaderTimestamp :: LocalTime
    }
    deriving (Show, Generic)

instance ToJSON CommitHeader where
    toJSON (CommitHeader commitID creationTs) =
        Aeson.object ["id" .= commitID, "creationTs" .= creationTs]

instance FromJSON CommitHeader where
    parseJSON = Aeson.withObject "CommitHeader" $ \v ->
        CommitHeader
            <$> v .: "id"
            <*> v .: "creationTs"

instance ToSchema CommitHeader where
    declareNamedSchema _ = do
        idSchema <- declareSchemaRef (Proxy :: Proxy CommitID)
        tsSchema <- declareSchemaRef (Proxy :: Proxy LocalTime)
        return $
            NamedSchema (Just "CommitHeader") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("id", idSchema)
                            , ("creationTs", tsSchema)
                            ]
                    & required .~ ["id", "creationTs"]

-- | metadata about a commit
data CommitInfo = CommitInfo
    { commitInfoAuthor :: UUID
    , commitInfoMessage :: Maybe Text
    , commitInfoParents :: [CommitID]
    }
    deriving (Show, Generic)

instance ToJSON CommitInfo where
    toJSON (CommitInfo author message parents) =
        Aeson.object
            [ "author" .= author
            , "message" .= message
            , "parents" .= parents
            ]

instance FromJSON CommitInfo where
    parseJSON = Aeson.withObject "CommitInfo" $ \v ->
        CommitInfo
            <$> v .: "author"
            <*> v .: "message"
            <*> v .: "parents"

instance ToSchema CommitInfo where
    declareNamedSchema _ = do
        authorSchema <- declareSchemaRef (Proxy :: Proxy UUID)
        messageSchema <- declareSchemaRef (Proxy :: Proxy (Maybe Text))
        parentSchema <- declareSchemaRef (Proxy :: Proxy (Maybe CommitRef))
        return $
            NamedSchema (Just "CommitInfo") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("author", authorSchema)
                            , ("message", messageSchema)
                            , ("parent", parentSchema)
                            ]
                    & required .~ ["author"]

-- | describes a parent-child relation between two commits
data CommitRel = CommitRel
    { commitRelParent :: CommitID
    , commitRelChild :: CommitID
    }

-- | information about a commits position in the commit graph.
data CommitNode = CommitNode
    { commitNodeID :: CommitID
    , commitNodeBase :: Maybe CommitID
    , commitNodeHeight :: Int32
    , commitNodeRootCommit :: Maybe CommitID
    }
