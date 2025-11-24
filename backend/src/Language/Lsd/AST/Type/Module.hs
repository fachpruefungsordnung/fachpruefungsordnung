module Language.Lsd.AST.Type.Module
    ( ModuleBlockType (..)
    , ModuleSchemaType (..)
    , CategoryType (..)
    , ModuleType (..)
    ) where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Text (TextType)

data ModuleBlockType
    = ModuleBlockType
        (TextType Void)
        ModuleSchemaType
        CategoryType

newtype ModuleSchemaType = ModuleSchemaType Keyword

data CategoryType = CategoryType Keyword ModuleType

newtype ModuleType = ModuleType Keyword
