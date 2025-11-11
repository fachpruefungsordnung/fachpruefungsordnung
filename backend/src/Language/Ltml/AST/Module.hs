module Language.Ltml.AST.Module
    ( ModuleBlock (..)
    , ModuleSchema (..)
    , Category (..)
    , Module (..)
    , Attribute (..)
    ) where

import Language.Ltml.AST.Text (RichTextTree)

data ModuleBlock = ModuleBlock ModuleSchema [Category]
    deriving (Show)

newtype ModuleSchema = ModuleSchema [Attribute]
    deriving (Show)

data Category = Category Attribute [Module]
    deriving (Show)

newtype Module = Module [Attribute]
    deriving (Show)

newtype Attribute = Attribute {unAttribute :: [RichTextTree]}
    deriving (Show)
