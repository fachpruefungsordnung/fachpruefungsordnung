module Docs.Tree
    ( Tree (..)
    , Edge (..)
    , TreeVersionID (..)
    , TreeVersion (..)
    , ExistingTreeVersion (..)
    , TreeVersionToC
    , TreeVersionFull
    , treeWithTextVersions
    ) where

import Data.Text (Text)
import Data.Time (LocalTime)
import Data.UUID (UUID)
import Docs.Text
    ( ExistingTextVersion
    , TextElement (textElementID)
    , TextElementID
    , TextElementVersion (TextElementVersion)
    )
import GHC.Int (Int32)

data Tree a
    = Node Text [Edge a]
    | Leaf a

data Edge a = Edge Text (Tree a)

newtype TreeVersionID = StructureVersionID
    { unTreeVersionID :: Int32
    }

data TreeVersion a = TreeVersion
    { treeVersionTimestamp :: LocalTime
    , treeVersionAuthor :: UUID
    , treeVersionRoot :: Tree a
    }

data ExistingTreeVersion a
    = ExistingTreeVersion
        TreeVersionID
        (TreeVersion a)

type TreeVersionToC = ExistingTreeVersion TextElement

type TreeVersionFull = ExistingTreeVersion TextElementVersion

instance Functor Tree where
    fmap f (Node header edge) = Node header $ (f <$>) <$> edge
    fmap f (Leaf x) = Leaf $ f x

instance Functor Edge where
    fmap f (Edge label tree) = Edge label $ f <$> tree

instance Functor TreeVersion where
    fmap f treeVersion =
        TreeVersion
            { treeVersionTimestamp = treeVersionTimestamp treeVersion
            , treeVersionAuthor = treeVersionAuthor treeVersion
            , treeVersionRoot = f <$> treeVersionRoot treeVersion
            }

instance Functor ExistingTreeVersion where
    fmap f (ExistingTreeVersion id_ treeVersion) =
        ExistingTreeVersion id_ $ f <$> treeVersion

treeWithTextVersions
    :: (Monad m)
    => (TextElementID -> m ExistingTextVersion)
    -- ^ (potentially effectful) function for obtaining a text version
    -> Tree TextElement
    -- ^ document structure tree
    -> m (Tree TextElementVersion)
    -- ^ document structure tree with concrete text versions
treeWithTextVersions getTextVersion = treeWithTextVersions'
  where
    treeWithTextVersions' (Leaf textElement) = do
        existingTextVersion <- getTextVersion $ textElementID textElement
        return $ Leaf $ TextElementVersion textElement existingTextVersion
    treeWithTextVersions' (Node metaData edges) = do
        edgesFull <- mapM mapEdge edges
        return $ Node metaData edgesFull
      where
        mapEdge (Edge label tree) = do
            treeFull <- treeWithTextVersions' tree
            return $ Edge label treeFull
