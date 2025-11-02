module Language.Ltml.AST.Module (ModuleSchema (..), Attribute (..), Module (..), ModuleBlock (..)) where

import Data.Text (Text)

newtype ModuleSchema = ModuleSchema [Attribute]
    deriving (Show)

newtype Attribute = Attribute Text
    deriving (Show)

-- TODO: Use  TextType instead of Attribute to support footnoteRefs etc.
newtype Module = Module [Attribute]
    deriving (Show)

data ModuleBlock = ModuleBlock ModuleSchema [Module]
    deriving (Show)
