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

import Language.Ltml.AST.Text (TableTextTree)
import Control.Monad.State (State, get, put, execState)
import Control.Monad
import Data.Array

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


type Matrix a = Array (Int, Int) a
type Visited = (Int, Int) -> Bool
type Value = (Int, Int)
type Position = (Int, Int)

-- Convert a list of lists to an array
listToArray :: [[a]] -> Matrix a
listToArray xss =
    let n = length xss
        m = if n == 0 then 0 else length (head xss)
    in array ((0,0), (n-1,m-1))
             [ ((i,j), (xss !! i) !! j) | i <- [0..n-1], j <- [0..m-1] ]

-- Convert array back to list of lists (optional)
arrayToList :: Matrix a -> [[a]]
arrayToList arr =
    let ((i0,j0),(in_,jn)) = bounds arr
    in [[ arr ! (i,j) | j <- [j0..jn]] | i <- [i0..in_]]

tableToArray :: Table_ -> Matrix Cell_
tableToArray (Table_ rows_) =
    let n = length rows_
        m = if n == 0 then 0 else length (let (Row_ cells) = rows_ !! 0 in cells)
    in array ((0,0), (n-1,m-1))
             [ ((i,j), (rows_ !! i) `getCell` j) | i <- [0..n-1], j <- [0..m-1] ]
  where
    getCell (Row_ cells) j = cells !! j


-- Your rewritten function
findEqualSubSquares :: Eq a => Matrix a -> Matrix Value
findEqualSubSquares matrix =
    let matrix' = array ((0,0),(n1,m1)) [((i,j), emptyEntry) | i <- [0..n1], j <- [0..m1]]
        visited0 (_,_) = False
    in snd $ execState process (visited0, matrix')
  where
    ((0,0),(n1,m1)) = bounds matrix
    n = n1 + 1
    m = m1 + 1
    emptyEntry = (0,0)

    updateVisited :: Position -> State (Visited, Matrix Value) ()
    updateVisited (i, j) = do
        (visited, mat) <- get
        let newVisited (x,y) = (y == i && x == j) || visited (x,y)
        put (newVisited, mat)

    updateMatrix :: Position -> Value -> State (Visited, Matrix Value) ()
    updateMatrix pos val = do
        (visited, mat) <- get
        let newMat = mat // [(pos, val)]
        put (visited, newMat)

    maxWidth (i, j) val = length $ takeWhile (\x -> x < m && matrix ! (i,x) == val) [j..m-1]

    maxHeight (i, j) w val =
        length $ takeWhile (\y -> y < n && all (\x -> matrix ! (y,x) == val) [j..j+w-1]) [i..n-1]

    process =
        forM_ [0..n-1] $ \i ->
          forM_ [0..m-1] $ \j -> do
            let pos = (i, j)
            (visited, _) <- get
            if visited pos
                then updateMatrix pos emptyEntry
                else do
                    let val = matrix ! pos
                    let w = maxWidth pos val
                    let h = maxHeight pos w val

                    forM_ [i..i+h-1] $ \y ->
                        forM_ [j..j+w-1] $ \x ->
                            updateVisited (x, y)

                    updateMatrix pos (w,h)