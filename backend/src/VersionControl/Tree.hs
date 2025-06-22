{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VersionControl.Tree
    ( Tree (..)
    , Ref (..)
    , TreeRef
    , Edge (..)
    , NodeWithMaybeRef (..)
    , NodeWithRef (..)
    , Node (..)
    , NodeID (..)
    , DataEdge (..)
    , editableTree
    , editableTreeRef
    , treeRefHash
    , dataEdges
    , hashedTree
    , hashTree
    , mkTree
    , mkEdge
    )
where

import Control.Applicative ((<|>))
import Control.Lens ((.~), (?~))
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , ToSchema (..)
    , declareSchemaRef
    , properties
    , required
    , type_
    )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics
import GHC.Int
import VersionControl.Hash (Hash (..), Hashable (..), Hashed (..))

-- | represents the id of a `Node`
newtype NodeID = NodeID
    { unNodeID :: Int32
    }
    deriving (Show, Generic)

instance ToJSON NodeID where
    toJSON = toJSON . unNodeID

instance FromJSON NodeID where
    parseJSON = fmap NodeID . parseJSON

instance ToSchema NodeID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int32)

instance Hashable NodeID where
    updateHash ctx nodeID = updateHash ctx $ unNodeID nodeID

-- | represents either a reference to an object in the database or the object itself
data Ref a b
    = Ref a
    | Value b
    deriving (Show, Generic)

instance Functor (Ref a) where
    fmap f (Value b) = Value $ f b
    fmap _ (Ref a) = Ref a

instance (ToJSON a, ToJSON b) => ToJSON (Ref a b) where
    toJSON (Ref ref) = Aeson.object ["ref" .= ref]
    toJSON (Value value) = toJSON value

instance (FromJSON a, FromJSON b) => FromJSON (Ref a b) where
    parseJSON v =
        Aeson.withObject "Ref" (\o -> Ref <$> o .: "ref") v
            <|> (Value <$> parseJSON v)

instance (ToSchema a, ToSchema b) => ToSchema (Ref a b)

-- | represents a specific document node version tree
data Tree a = Tree a [Edge a] deriving (Show)

instance Functor Tree where
    fmap f (Tree a edges) = Tree (f a) $ fmap f <$> edges

instance (ToJSON a) => ToJSON (Tree a) where
    toJSON (Tree node edges) =
        Aeson.object ["node" .= node, "children" .= edges]

instance (FromJSON a) => FromJSON (Tree a) where
    parseJSON = Aeson.withObject "Tree" $ \v ->
        Tree
            <$> v .: "node"
            <*> v .: "edges"

instance (ToSchema a) => ToSchema (Tree a) where
    declareNamedSchema _ = do
        nodeSchema <- declareSchemaRef (Proxy :: Proxy a)
        edgesSchema <- declareSchemaRef (Proxy :: Proxy [Edge a])
        return $
            NamedSchema (Just "Tree") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("node", nodeSchema)
                            , ("edges", edgesSchema)
                            ]
                    & required .~ ["node", "edges"]

-- | the resulting tree does not require any node to have an id specified
editableTree :: Tree (Hashed NodeWithRef) -> Tree NodeWithMaybeRef
editableTree = ((\(Hashed _ t) -> toNodeWithMaybeRef t) <$>)

-- | the resulting tree ref does not require any node to have an id specified
editableTreeRef :: TreeRef (Hashed NodeWithRef) -> TreeRef NodeWithMaybeRef
editableTreeRef = (editableTree <$>)

-- | represents an edge of a tree
data Edge a = Edge Text (TreeRef a) deriving (Show)

instance Functor Edge where
    fmap f (Edge label treeRef) = Edge label $ fmap f <$> treeRef

instance (ToJSON a) => ToJSON (Edge a) where
    toJSON (Edge label node) =
        Aeson.object ["title" .= label, "child" .= node]

instance (FromJSON a) => FromJSON (Edge a) where
    parseJSON = Aeson.withObject "Edge" $ \v ->
        Edge
            <$> v .: "title"
            <*> v .: "child"

instance (ToSchema a) => ToSchema (Edge a) where
    declareNamedSchema _ = do
        labelSchema <- declareSchemaRef (Proxy :: Proxy Text)
        refSchema <- declareSchemaRef (Proxy :: Proxy (TreeRef a))
        return $
            NamedSchema (Just "Edge") $
                mempty
                    & type_ ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("title", labelSchema)
                            , ("child", refSchema)
                            ]
                    & required .~ ["title", "child"]

-- | a reference to a tree
type TreeRef a = Ref Hash (Tree a)

-- | extract the 'Hash' from a hashed 'TreeRef'
treeRefHash :: TreeRef (Hashed a) -> Hash
treeRefHash (Ref ref) = ref
treeRefHash (Value (Tree (Hashed ref _) _)) = ref

-- | convenience constructor for an 'Edge'
mkEdge :: Text -> Tree NodeWithMaybeRef -> Edge NodeWithMaybeRef
mkEdge label tree = Edge label $ Value tree

-- | convenience constructor for a 'Tree'
mkTree :: Node -> [Edge NodeWithMaybeRef] -> Tree NodeWithMaybeRef
mkTree node = Tree (NodeWithMaybeRef Nothing node)

-- | construct a hashed 'Tree' from a node and its already hashed edges
hashedTree
    :: NodeWithRef
    -> [Edge (Hashed NodeWithRef)]
    -> Tree (Hashed NodeWithRef)
hashedTree self children =
    Tree
        ( Hashed
            (Hash (SHA1.finalize (foldr hashEdge (updateHash SHA1.init self) children)))
            self
        )
        children
  where
    hashEdge (Edge label childEdge) ctx =
        updateHash ctx label
            & flip updateHash (treeRefHash childEdge)

-- | construct a hashed 'Tree'
hashTree :: Tree NodeWithRef -> Tree (Hashed NodeWithRef)
hashTree (Tree self children) = hashedTree self $ hashEdge <$> children
  where
    hashEdge (Edge label treeRef) = Edge label $ case treeRef of
        Value tree -> Value (hashTree tree)
        Ref ref -> Ref ref

-- | a node version for a node which might not yet exist
data NodeWithMaybeRef
    = NodeWithMaybeRef (Maybe NodeID) Node
    deriving (Show)

instance ToJSON NodeWithMaybeRef where
    toJSON (NodeWithMaybeRef ref node) =
        Aeson.object
            [ "id" .= ref
            , "kind" .= nodeKind node
            , "content" .= nodeContent node
            ]

instance FromJSON NodeWithMaybeRef where
    parseJSON = Aeson.withObject "NodeWithMaybeRef" $ \v ->
        NodeWithMaybeRef
            <$> v .: "id"
            <*> ( Node
                    <$> v .: "kind"
                    <*> v .: "content"
                )

instance ToSchema NodeWithMaybeRef where
    declareNamedSchema _ = do
        idSchema <- declareSchemaRef (Proxy :: Proxy (Maybe NodeID))
        kindSchema <- declareSchemaRef (Proxy :: Proxy String)
        contentSchema <- declareSchemaRef (Proxy :: Proxy Node)
        return
            $ NamedSchema
                (Just "NodeWithMaybeRef")
            $ mempty
                & type_
                    ?~ OpenApiObject
                & properties
                    .~ InsOrd.fromList
                        [ ("id", idSchema)
                        , ("kind", kindSchema)
                        , ("content", contentSchema)
                        ]
                & required .~ ["kind", "content"]

-- | a node version for a node which is guaranteed to already exist
data NodeWithRef
    = NodeWithRef NodeID Node
    deriving (Show, Generic)

instance ToJSON NodeWithRef where
    toJSON (NodeWithRef ref node) =
        Aeson.object
            [ "id" .= ref
            , "kind" .= nodeKind node
            , "content" .= nodeContent node
            ]

instance FromJSON NodeWithRef where
    parseJSON = Aeson.withObject "NodeWithRef" $ \v ->
        NodeWithRef
            <$> v .: "id"
            <*> ( Node
                    <$> v .: "kind"
                    <*> v .: "content"
                )

instance ToSchema NodeWithRef where
    declareNamedSchema _ = do
        idSchema <- declareSchemaRef (Proxy :: Proxy NodeID)
        kindSchema <- declareSchemaRef (Proxy :: Proxy String)
        contentSchema <- declareSchemaRef (Proxy :: Proxy Node)
        return
            $ NamedSchema
                (Just "NodeWithRef")
            $ mempty
                & type_
                    ?~ OpenApiObject
                & properties
                    .~ InsOrd.fromList
                        [ ("id", idSchema)
                        , ("kind", kindSchema)
                        , ("content", contentSchema)
                        ]
                & required .~ ["id", "kind", "content"]

instance Hashable NodeWithRef where
    updateHash ctx (NodeWithRef ref node) =
        updateHash ctx ref
            & flip updateHash (nodeKind node)
            & flip updateHash (nodeContent node)

toNodeWithMaybeRef :: NodeWithRef -> NodeWithMaybeRef
toNodeWithMaybeRef (NodeWithRef ref node) = NodeWithMaybeRef (Just ref) node

-- | a node version
data Node = Node
    { nodeKind :: Text
    , nodeContent :: Maybe Text
    }
    deriving (Show, Generic)

instance ToJSON Node

instance FromJSON Node

instance ToSchema Node

-- | a relational-style edge
data DataEdge = DataEdge
    { dataEdgeParent :: Hash
    , dataEdgeChild :: Hash
    , dataEdgeChildPosition :: Int32
    , dataEdgeChildTitle :: Text
    }

-- | obtains a list of relational-style edges from a tree.
dataEdges :: Tree (Hashed NodeWithRef) -> [DataEdge]
dataEdges (Tree (Hashed parentHash _) children) =
    zipWith fromEdge [1, 2 ..] children
  where
    fromEdge position (Edge label child) =
        DataEdge
            { dataEdgeParent = parentHash
            , dataEdgeChild = treeRefHash child
            , dataEdgeChildPosition = position
            , dataEdgeChildTitle = label
            }
