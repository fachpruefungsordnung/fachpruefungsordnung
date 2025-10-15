{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Docs.MetaTree
-- Description : Tree With Metadata
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module contains the definition of a tree with additional meta data
-- such as labels, titles and allowed edit actions.
module Docs.MetaTree
    ( MetaNode (..)
    , MetaTree (..)
    , Meta (..)
    , TocEntry (..)
    , TreeWithMetaData (..)
    , TreeRevisionWithMetaData (..)
    ) where

import Data.Text (Text)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.OpenApi (ToSchema)
import Docs.Tree (NodeHeader)
import Docs.TreeRevision (TreeRevisionHeader)
import qualified Language.Lsd.AST.Common as LSD
import qualified Language.Lsd.AST.Type as LSD
import qualified Language.Ltml.HTML.Common as HTML

-- | Like "Docs.TreeRevision.TreeRevision", but with ✨metadata✨.
data TreeRevisionWithMetaData a
    = TreeRevisionWithMetaData
    { revisionHeader :: TreeRevisionHeader
    -- ^ the tree revision meta data
    , revision :: TreeWithMetaData a
    -- ^ tre revisions content
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (TreeRevisionWithMetaData a)

instance (FromJSON a) => FromJSON (TreeRevisionWithMetaData a)

instance (ToSchema a) => ToSchema (TreeRevisionWithMetaData a)

instance Functor TreeRevisionWithMetaData where
    fmap f rev = rev {revision = f <$> revision rev}

-- | Wrapper around the root of a tree, but with ✨metadata✨.
data TreeWithMetaData a
    = TreeWithMetaData
    { root :: Meta a
    -- ^ the root of the tree
    , metaMap :: Map LSD.FullTypeName LSD.ProperTypeMeta
    -- ^ map of allowed edit actions i guess
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (TreeWithMetaData a)

instance (FromJSON a) => FromJSON (TreeWithMetaData a)

instance (ToSchema a) => ToSchema (TreeWithMetaData a)

instance Functor TreeWithMetaData where
    fmap f tree' = tree' {root = f <$> root tree'}

-- | Like "Docs.Tree.Node", but with ✨metadata✨.
data MetaNode a
    = MetaNode
    { header :: NodeHeader
    -- ^ metadata for the node
    , children :: [Meta a]
    -- ^ children of the node
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (MetaNode a)

instance (FromJSON a) => FromJSON (MetaNode a)

instance (ToSchema a) => ToSchema (MetaNode a)

-- | Information on how the node should be displayed in the Table of Contents.
data TocEntry = TocEntry
    { label :: Maybe Text
    -- ^ the label of the node
    , title :: HTML.Result (Maybe Text)
    -- ^ the title the node should by displayed as in the ToC
    }
    deriving (Generic)

instance ToJSON TocEntry

instance FromJSON TocEntry

instance ToSchema TocEntry

-- | Wrapper around a @MetaTree@, but with ✨metadata✨.
data Meta a = Meta
    { meta :: TocEntry
    , tree :: MetaTree a
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (Meta a)

instance (FromJSON a) => FromJSON (Meta a)

instance (ToSchema a) => ToSchema (Meta a)

-- | Like "Docs.Tree.Tree", but with ✨metadata✨.
data MetaTree a
    = MetaTree (MetaNode a)
    | MetaLeaf a
    deriving (Generic)

instance (ToJSON a) => ToJSON (MetaTree a)

instance (FromJSON a) => FromJSON (MetaTree a)

instance (ToSchema a) => ToSchema (MetaTree a)

instance Functor MetaNode where
    fmap f (MetaNode nodeHeader edge) = MetaNode nodeHeader $ (f <$>) <$> edge

instance Functor MetaTree where
    fmap f (MetaTree node) = MetaTree $ f <$> node
    fmap f (MetaLeaf x) = MetaLeaf $ f x

instance Functor Meta where
    fmap f (Meta label' tree') = Meta label' $ f <$> tree'

instance Foldable Meta where
    foldMap f (Meta _ tree') = foldMap f tree'

instance Foldable MetaNode where
    foldMap f (MetaNode _ edges) = foldMap (foldMap f) edges

instance Foldable MetaTree where
    foldMap f (MetaLeaf a) = f a
    foldMap f (MetaTree node) = foldMap f node

instance Traversable Meta where
    traverse f (Meta label' tree') = Meta label' <$> traverse f tree'

instance Traversable MetaNode where
    traverse f (MetaNode label' edges) = MetaNode label' <$> traverse (traverse f) edges

instance Traversable MetaTree where
    traverse f (MetaLeaf a) = MetaLeaf <$> f a
    traverse f (MetaTree node) = MetaTree <$> traverse f node
