module Language.Ltml.AST.Module (ModuleBlock (..), ModuleSchema (..), Category (..), Module (..), Attribute (..)) where

import Data.Text (Text)

data ModuleBlock = ModuleBlock ModuleSchema [Category]
    deriving (Show)

newtype ModuleSchema = ModuleSchema [Attribute]
    deriving (Show)

data Category = Category Attribute [Module]
    deriving (Show)

-- TODO: Use  TextType instead of Attribute to support footnoteRefs etc.
newtype Module = Module [Attribute]
    deriving (Show)

newtype Attribute = Attribute {unAttribute :: Text}
    deriving (Show)
