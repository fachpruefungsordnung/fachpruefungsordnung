module Docs.Tree
    ( Tree (..)
    , Edge (..)
    , Node (..)
    , NodeHeader (..)
    , withTextRevisions
    ) where

import Data.Functor ((<&>))
import Data.Text (Text)

import Docs.TextElement (TextElement, TextElementID)
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision)
    , TextRevision
    )
import DocumentManagement.Hash (Hashable (..))

data NodeHeader = NodeHeader
    { headerKind :: Text
    , headerType :: Text
    }

instance Hashable NodeHeader where
    updateHash ctx header =
        updateHash (updateHash ctx (headerType header)) $
            headerKind header

data Node a = Node NodeHeader [Edge a]

data Tree a
    = Tree (Node a)
    | Leaf a

data Edge a = Edge Text (Tree a)

instance Functor Node where
    fmap f (Node header edge) = Node header $ (f <$>) <$> edge

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
