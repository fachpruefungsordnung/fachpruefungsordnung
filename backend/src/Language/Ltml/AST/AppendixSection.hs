module Language.Ltml.AST.AppendixSection
    ( AppendixSection (..)
    )
where

import Language.Lsd.AST.Type.AppendixSection (AppendixSectionFormat)
import Language.Ltml.AST.Document (Document)
import Language.Ltml.AST.Node (Node)

data AppendixSection
    = AppendixSection
        AppendixSectionFormat
        [Node Document]
    deriving (Show)
