module Language.Lsd.AST.Type.Module
    ( ModuleBlockType (..)
    , ModuleSchemaType (..)
    , CategoryType (..)
    , ModuleType (..)
    ) where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Table (CellFormat)
import Language.Lsd.AST.Type.Text (TextType)

data ModuleBlockType
    = ModuleBlockType
        (TextType Void)
        ModuleSchemaType
        CategoryType

data ModuleSchemaType = ModuleSchemaType Keyword CellFormat

data CategoryType = CategoryType Keyword CellFormat ModuleType

data ModuleType = ModuleType Keyword CellFormat
