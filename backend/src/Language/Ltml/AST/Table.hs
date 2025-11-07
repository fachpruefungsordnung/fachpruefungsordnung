{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.AST.Table
    ( Table (..)
    , Row (..)
    , Cell (..)
    )
where

import Language.Ltml.AST.Text (TableTextTree)

-- the internal representation of a table
newtype Table = Table [Row]

instance Show Table where
    show (Table rows) = unlines $ map show rows

newtype Row = Row [Cell]
    deriving (Show)

data Cell = Cell
    { cellContent :: [TableTextTree]
    , cellColSpan :: Int
    , cellRowSpan :: Int
    }
    deriving (Show)
