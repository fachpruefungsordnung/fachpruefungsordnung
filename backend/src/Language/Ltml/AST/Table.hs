{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ltml.AST.Table
    ( Table (..)
    , Row (..)
    , Cell (..)
    , mergeTable_
    , Table_ (..)
    , Row_ (..)
    , Cell_ (..)
    )
where

import Language.Ltml.AST.Text (TableTextTree, TextTree (Word))
import Control.Monad.State (State)

newtype Table_ = Table_ [Row_]

instance Show Table_ where
    show (Table_ rows) = unlines $ map show rows

newtype Row_ = Row_ [Cell_]
    deriving (Show)

data Cell_ = Cell_ [TableTextTree] | EmptyCell | MergeLeft | MergeUp
    deriving (Show)

mergeTable_ :: Table_ -> Table
mergeTable_ (Table_ rows_) = undefined
  where
    initState = []
    
    mergeHorizontal :: [Cell_] -> State [Cell] [Cell]
    mergeHorizontal [] = return []
    mergeHorizontal (EmptyCell:xs) = do
        rest <- mergeHorizontal xs
        pure (Cell [] 1 1 : rest)
    mergeHorizontal _ = undefined
  


newtype Table = Table [Row]

instance Show Table where
    show (Table rows) = unlines $ map show rows

newtype Row = Row [Cell]
    deriving (Show)

data Cell = Cell
  { cellContent :: [TableTextTree]
  , cellColSpan :: Int
  , cellRowSpan :: Int
  } deriving (Show)

