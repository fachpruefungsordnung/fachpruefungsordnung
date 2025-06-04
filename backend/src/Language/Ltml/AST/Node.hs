module Language.Ltml.AST.Node
    ( Node (..)
    )
where

import Language.Ltml.AST.Label (Label)

data Node a = Node (Maybe Label) a
    deriving (Show)
