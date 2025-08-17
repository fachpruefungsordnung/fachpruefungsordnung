module Language.Ltml.AST.SimpleSection
    ( SimpleSection (..)
    )
where

import Language.Lsd.AST.Type.SimpleSection (SimpleSectionFormat)
import Language.Ltml.AST.SimpleParagraph (SimpleParagraph)

data SimpleSection
    = SimpleSection
        SimpleSectionFormat
        [SimpleParagraph]
    deriving (Show)
