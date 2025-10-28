module Language.Ltml.AST.Module (Schema (..), Attribute (..), Module (..)) where

import Data.Text (Text)

newtype Schema = Schema [Attribute]

newtype Attribute = Attribute Text

newtype Module = Module [Text]
