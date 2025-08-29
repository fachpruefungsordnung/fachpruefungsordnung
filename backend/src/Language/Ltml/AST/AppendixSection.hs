module Language.Ltml.AST.AppendixSection
    ( AppendixSection (..)
    )
where

import Language.Lsd.AST.Type.AppendixSection (AppendixSectionFormat)
import Language.Ltml.AST.Document (Document)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.Common (Flagged')

data AppendixSection
    = AppendixSection
        AppendixSectionFormat
        [Flagged' (Node Document)]
    deriving (Show)
