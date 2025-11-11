module Language.Lsd.AST.Type.Module
    ( ModuleBlockType (..)
    , ModuleSchemaType (..)
    , CategoryType (..)
    , ModuleType (..)
    ) where

import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Lsd.AST.Type.Text (TextType)

data ModuleBlockType
    = ModuleBlockType
        Keyword
        -- ^ Seperation Keyword for Category and Module Attribute lists
        (TextType EnumType)
        ModuleSchemaType
        CategoryType

newtype ModuleSchemaType = ModuleSchemaType Keyword

data CategoryType = CategoryType Keyword ModuleType

newtype ModuleType = ModuleType Keyword
