module Language.Ltml.AST.Table
    ( Table (..)
    )
where

import Data.Void (Void)

newtype Table = Table Void
    deriving (Show)
