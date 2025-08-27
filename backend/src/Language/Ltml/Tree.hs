module Language.Ltml.Tree
    ( FlaggedTree
    , TypedTree (..)
    , Tree (..)
    )
where

import Data.Text (Text)
import Language.Lsd.AST.Common (KindName, TypeName)
import Language.Ltml.Common (Flagged)

type FlaggedTree = Flagged TypedTree

-- | A raw representation of an LTML tree.
data TypedTree
    = TypedTree
        KindName
        TypeName
        Tree
    deriving (Show)

-- | Auxiliary LTML tree, without kind & type information in the head.
data Tree
    = Tree
        (Maybe Text)
        [FlaggedTree]
    | Leaf Text
    deriving (Show)
