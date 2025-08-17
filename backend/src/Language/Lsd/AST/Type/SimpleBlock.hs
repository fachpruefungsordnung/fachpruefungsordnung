module Language.Lsd.AST.Type.SimpleBlock
    ( SimpleBlockType (..)
    , PreSimpleBlockType (..)
    )
where

import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.SimpleRegex (Disjunction)
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphType)
import Language.Lsd.AST.Type.Table (TableType)

-- | A simple block type is basically a union of types.
data SimpleBlockType
    = SimpleBlockType
        SimpleParagraphType
        (Disjunction TableType)

data PreSimpleBlockType
    = PreSimpleBlockType
        TypeName
        (Disjunction TypeName)
