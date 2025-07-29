{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Docs.Tree
    ( Tree (..)
    , Edge (..)
    , Node (..)
    , NodeHeader (..)
    , withTextRevisions
    ) where

import Data.Functor ((<&>))
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
    , Schema (..)
    , ToSchema (..)
    , declareSchemaRef
    , enum_
    , oneOf
    , properties
    , required
    , type_
    )

import Docs.TextElement (TextElement, TextElementID)
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision)
    , TextRevision
    )
import DocumentManagement.Hash (Hashable (..))

-- | Contains metdata for a tree node.
data NodeHeader = NodeHeader
    { headerKind :: Text
    , headerType :: Text
    }
    deriving (Show, Generic)

instance Hashable NodeHeader where
    updateHash ctx nodeHeader =
        updateHash (updateHash ctx (headerType nodeHeader)) $
            headerKind nodeHeader

instance ToJSON NodeHeader

instance FromJSON NodeHeader

instance ToSchema NodeHeader

-- | A node of a tree.
data Node a = Node
    { header :: NodeHeader
    , children :: [Edge a]
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (Node a)

instance (FromJSON a) => FromJSON (Node a)

instance (ToSchema a) => ToSchema (Node a)

-- | A tree. Either a tree or a leaf.
data Tree a
    = Tree (Node a)
    | Leaf a

instance (ToJSON a) => ToJSON (Tree a) where
    toJSON (Tree node) = Aeson.object ["type" .= ("tree" :: Text), "node" .= node]
    toJSON (Leaf leaf) = Aeson.object ["type" .= ("leaf" :: Text), "leaf" .= leaf]

instance (FromJSON a) => FromJSON (Tree a) where
    parseJSON = Aeson.withObject "Tree" $ \obj -> do
        ty <- obj .: "type" :: Parser Text
        case ty of
            "tree" -> Tree <$> obj .: "node"
            "leaf" -> Leaf <$> obj .: "leaf"
            _ -> fail $ "Unknown Tree type: " ++ show ty

instance (ToSchema a) => ToSchema (Tree a) where
    declareNamedSchema _ = do
        nodeSchema <- declareSchemaRef (Proxy :: Proxy (Node a))
        leafSchema <- declareSchemaRef (Proxy :: Proxy a)

        return $
            NamedSchema (Just "Tree") $
                mempty
                    & type_ ?~ OpenApiObject
                    & oneOf
                        ?~ [ Inline $
                                mempty
                                    & type_ ?~ OpenApiObject
                                    & properties
                                        .~ InsOrd.fromList
                                            [ ("type", Inline $ schemaConstText "tree")
                                            , ("node", nodeSchema)
                                            ]
                                    & required .~ ["type", "with"]
                           , Inline $
                                mempty
                                    & type_ ?~ OpenApiObject
                                    & properties
                                        .~ InsOrd.fromList
                                            [ ("type", Inline $ schemaConstText "leaf")
                                            , ("leaf", leafSchema)
                                            ]
                                    & required .~ ["type", "newRevision"]
                           ]
      where
        schemaConstText :: Text -> Schema
        schemaConstText val =
            mempty
                & type_ ?~ OpenApiString
                & enum_ ?~ [toJSON val]

-- | An edge (from a node) to a node or leaf.
data Edge a = Edge
    { title :: Text
    , content :: Tree a
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (Edge a)

instance (FromJSON a) => FromJSON (Edge a)

instance (ToSchema a) => ToSchema (Edge a)

instance Functor Node where
    fmap f (Node nodeHeader edge) = Node nodeHeader $ (f <$>) <$> edge

instance Functor Tree where
    fmap f (Tree node) = Tree $ f <$> node
    fmap f (Leaf x) = Leaf $ f x

instance Functor Edge where
    fmap f (Edge label tree) = Edge label $ f <$> tree

instance Foldable Edge where
    foldMap f (Edge _ tree) = foldMap f tree

instance Foldable Node where
    foldMap f (Node _ edges) = foldMap (foldMap f) edges

instance Foldable Tree where
    foldMap f (Leaf a) = f a
    foldMap f (Tree node) = foldMap f node

instance Traversable Edge where
    traverse f (Edge label tree) = Edge label <$> traverse f tree

instance Traversable Node where
    traverse f (Node label edges) = Node label <$> traverse (traverse f) edges

instance Traversable Tree where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Tree node) = Tree <$> traverse f node

-- | Takes a tree and emplaces concrete text revision.
-- | The text revions are obtained via the specified getter function.
withTextRevisions
    :: (Monad m)
    => (TextElementID -> m (Maybe TextRevision))
    -- ^ (potentially effectful) function for obtaining a text revision
    -> Node TextElement
    -- ^ document structure tree
    -> m (Node TextElementRevision)
    -- ^ document structure tree with concrete text revisions
withTextRevisions getTextRevision = withTextRevisions'
  where
    withTextRevisions' (Node metaData edges) = mapM mapEdge edges <&> Node metaData
    treeWithTextRevisions (Tree node) = withTextRevisions' node <&> Tree
    treeWithTextRevisions (Leaf textElement) =
        getTextRevision (TextElement.identifier textElement)
            <&> Leaf . TextElementRevision textElement
    mapEdge (Edge label tree) = treeWithTextRevisions tree <&> Edge label
