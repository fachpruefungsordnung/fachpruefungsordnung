module Language.Ltml.AST.SimpleBlock
    ( SimpleBlock (..)
    )
where

import Language.Ltml.AST.SimpleParagraph (SimpleParagraph)
import Language.Ltml.AST.Table (Table)

data SimpleBlock
    = SimpleParagraphBlock SimpleParagraph
    | TableBlock Table
    deriving (Show)
