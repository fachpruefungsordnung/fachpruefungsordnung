{-# LANGUAGE ScopedTypeVariables #-}

module Language.Ltml.Tree
    ( FlaggedTree
    , TypedTree (..)
    , Tree (..)
    , flaggedTreeMap
    , FlaggedInputTree
    , TypedInputTree
    , InputTree
    , FlaggedInputTree'
    , TypedInputTree'
    , InputTree'
    , FlaggedMetaTree
    , TypedMetaTree
    , MetaTree
    )
where

import Data.Text (Text)
import Language.Lsd.AST.Common (KindName, TypeName)
import Language.Ltml.Common (Flagged, flagMap)
import Language.Ltml.HTML.Common (RenderedTocEntry)

type FlaggedTree flag a b = Flagged flag (TypedTree flag a b)

data TypedTree flag a b
    = TypedTree
        KindName
        TypeName
        (Tree flag a b)
    deriving (Show)

data Tree flag a b
    = Tree
        a
        [FlaggedTree flag a b]
    | Leaf b
    deriving (Show)

flaggedTreeMap
    :: forall fl fl' a a' b b'
     . (fl -> fl')
    -> (a -> a')
    -> (b -> b')
    -> FlaggedTree fl a b
    -> FlaggedTree fl' a' b'
flaggedTreeMap flagF innerF leafF = flaggedTreeF
  where
    flaggedTreeF :: FlaggedTree fl a b -> FlaggedTree fl' a' b'
    flaggedTreeF tree = typedTreeF <$> flagMap flagF tree

    typedTreeF :: TypedTree fl a b -> TypedTree fl' a' b'
    typedTreeF (TypedTree k t tree) = TypedTree k t $ treeF tree

    treeF :: Tree fl a b -> Tree fl' a' b'
    treeF (Tree x trees) = Tree (innerF x) $ map flaggedTreeF trees
    treeF (Leaf leaf) = Leaf $ leafF leaf

type FlaggedInputTree flag = FlaggedTree flag (Maybe Text) Text
type TypedInputTree flag = TypedTree flag (Maybe Text) Text
type InputTree flag = Tree flag (Maybe Text) Text

-- | A tree with textual nodes that can be parsed to obtain an LTML tree.
--   See `Language.Ltml.Common.Flagged'` on the semantics of the boolean flag.
type FlaggedInputTree' = FlaggedInputTree Bool

type TypedInputTree' = TypedInputTree Bool
type InputTree' = InputTree Bool

-- | A tree containing metadata, to be sent to the frontend.
--   The type parameter is typically an identifier type.
type FlaggedMetaTree id = FlaggedTree id RenderedTocEntry RenderedTocEntry

type TypedMetaTree id = TypedTree id RenderedTocEntry RenderedTocEntry
type MetaTree id = Tree id RenderedTocEntry RenderedTocEntry
