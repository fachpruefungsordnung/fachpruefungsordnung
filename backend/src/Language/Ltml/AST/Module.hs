module Language.Ltml.AST.Module
    ( ModuleBlock (..)
    , ModuleTable (..)
    , ModuleSchema (..)
    , Category (..)
    , Module (..)
    , Attribute (..)
    ) where

import Language.Ltml.AST.Text (TableTextTree)

data ModuleBlock = ModuleBlock ModuleSchema ModuleTable
    deriving (Show)

data ModuleTable
    = Categorized [Category]
    | Plain [Module]
    deriving (Show)

newtype ModuleSchema = ModuleSchema [Attribute]
    deriving (Show)

data Category = Category Attribute [Module]
    deriving (Show)

newtype Module = Module {unModule :: [Attribute]}
    deriving (Show)

newtype Attribute = Attribute {unAttribute :: [TableTextTree]}
    deriving (Show)
