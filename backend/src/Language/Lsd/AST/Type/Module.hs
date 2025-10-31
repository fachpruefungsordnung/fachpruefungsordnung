module Language.Lsd.AST.Type.Module (ModuleSchemaType (..), ModuleType (..)) where

import Language.Lsd.AST.Common (Keyword)

newtype ModuleSchemaType = ModuleSchemaType Keyword

newtype ModuleType = ModuleType Keyword
