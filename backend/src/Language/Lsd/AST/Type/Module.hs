module Language.Lsd.AST.Type.Module (ModuleBlockType (..), ModuleSchemaType (..), ModuleType (..)) where

import Language.Lsd.AST.Common (Keyword)

data ModuleBlockType = ModuleBlockType ModuleSchemaType ModuleType

newtype ModuleSchemaType = ModuleSchemaType Keyword

newtype ModuleType = ModuleType Keyword
