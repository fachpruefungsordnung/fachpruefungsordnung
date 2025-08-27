module Language.Ltml.Tree
    ( TypedTree (..)
    , Tree (..)
    )
where

import Data.Text (Text)
import Language.Lsd.AST.Common (KindName, TypeName)
import Language.Ltml.Common (Flagged)

-- | A raw representation of an LTML tree.
data TypedTree
    = TypedTree
        KindName
        TypeName
        Tree

-- | Auxiliary LTML tree, without kind & type information in the head.
data Tree
    = Tree
        (Maybe (Flagged Text))
        [TypedTree]
    | Leaf (Flagged Text)
