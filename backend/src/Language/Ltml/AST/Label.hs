module Language.Ltml.AST.Label
    ( Label (..)
    )
where

import Data.Text (Text)

newtype Label = Label {unLabel :: Text}
    deriving (Show, Eq, Ord)
