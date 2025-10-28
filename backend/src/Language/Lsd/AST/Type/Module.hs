module Language.Lsd.AST.Type.Module (SchemaType (..), ModuleType (..)) where

import Language.Lsd.AST.Common (Keyword)

newtype SchemaType = SchemaType Keyword

newtype ModuleType = ModuleType Keyword
