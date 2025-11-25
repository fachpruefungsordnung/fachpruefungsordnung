module Language.Lsd.AST.Type.SimpleBlock
    ( SimpleBlockType (..)
    )
where

import Language.Lsd.AST.Type (NamedType)
import Language.Lsd.AST.Type.Module (ModuleBlockType)
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphType)
import Language.Lsd.AST.Type.Table (TableType)

-- | A simple block type is basically a union of types.
data SimpleBlockType
    = SimpleBlockType
        (NamedType SimpleParagraphType)
        (NamedType TableType)
        (NamedType ModuleBlockType)
