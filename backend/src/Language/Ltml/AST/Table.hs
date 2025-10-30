module Language.Ltml.AST.Table
    ( Table (..)
    , Row (..)
    , Cell (..)
    )
where

import Data.Text (Text)

newtype Table = Table [Row]

instance Show Table where
    show (Table rows) = unlines $ map show rows

newtype Row = Row [Cell]
    deriving (Show)

newtype Cell = Cell Text
    deriving (Show)
