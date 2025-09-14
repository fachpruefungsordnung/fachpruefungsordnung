{-# LANGUAGE DeriveGeneric #-}

module Docs.MetaTree
    ( MetaNode (..)
    , MetaTree (..)
    , Meta (..)
    , TocEntry (..)
    , TreeWithMetaData (..)
    ) where

import Data.Text (Text)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Docs.Tree (NodeHeader)

data TreeWithMetaData a
    = TreeWithMetaData
    { root :: Meta a
    , metaMap :: String -- TODO!
    }

data MetaNode a
    = MetaNode
    { header :: NodeHeader
    , children :: [Meta a]
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (MetaNode a)

instance (FromJSON a) => FromJSON (MetaNode a)

instance (ToSchema a) => ToSchema (MetaNode a)

data TocEntry = TocEntry
    { label :: Maybe Text
    , title :: Maybe Text
    }
    deriving (Generic)

instance ToJSON TocEntry

instance FromJSON TocEntry

instance ToSchema TocEntry

data Meta a = Meta
    { meta :: TocEntry
    , tree :: MetaTree a
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (Meta a)

instance (FromJSON a) => FromJSON (Meta a)

instance (ToSchema a) => ToSchema (Meta a)

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
