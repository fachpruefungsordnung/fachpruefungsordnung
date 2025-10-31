module Language.Ltml.AST.Module (ModuleSchema (..), Attribute (..), Module (..), ModuleBlock (..)) where

import Data.Text (Text)

newtype ModuleSchema = ModuleSchema [Attribute]

newtype Attribute = Attribute Text

-- TODO: Use  TextType instead of Attribute to support footnoteRefs etc.
newtype Module = Module [Attribute]

data ModuleBlock = ModuleBlock ModuleSchema [Module]
