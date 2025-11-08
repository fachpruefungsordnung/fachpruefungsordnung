module Language.Lsd.AST.Type.Module
    ( ModuleBlockType (..)
    , ModuleSchemaType (..)
    , CategoryType (..)
    , ModuleType (..)
    ) where

import Language.Lsd.AST.Common (Keyword)

data ModuleBlockType = ModuleBlockType Keyword Keyword ModuleSchemaType CategoryType

newtype ModuleSchemaType = ModuleSchemaType Keyword

data CategoryType = CategoryType Keyword ModuleType

newtype ModuleType = ModuleType Keyword
