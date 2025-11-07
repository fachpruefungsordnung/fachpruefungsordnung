{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ltml.AST.Table
    ( Table (..)
    , Row (..)
    , Cell (..)
    , Table' (..)
    , Row' (..)
    , Cell' (..)
    , mergeCells
    )
where

import Language.Ltml.AST.Text (TableTextTree, TextTree (Word))
import Control.Monad.State (State, get, put, execState)
import Control.Monad ( forM_ )
import Data.Array ( Array, (!), (//), array, bounds )

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
  } deriving (Show)

-- the rawly parsed representation of a table
newtype Table' = Table' {unTable' :: [Row']}
    deriving (Show)

newtype Row' = Row' {unRow' :: [Cell']}
    deriving (Show)

data Cell' = Cell' [TableTextTree] | EmptyCell | MergeLeft | MergeUp
    deriving (Show)

instance Eq Cell' where
    (Cell' _) == (Cell' _) = True
    EmptyCell == EmptyCell = True
    MergeLeft == MergeLeft = True
    MergeUp == MergeUp = True
    _ == _ = False


-- everything to merge cells and compute spans

type Matrix a = Array (Int, Int) a
type Visited = (Int, Int) -> Bool
type Position = (Int, Int)

mergeCells :: Table' -> Table
mergeCells table =
    let matrix' = array ((0,0),(n,m)) [((i,j), emptyEntry) | i <- [0..n], j <- [0..m]]
        visited0 (_,_) = False
    in arrayToTable $ snd $ execState process (visited0, matrix')
  where
    table' = padTable table
    rawMatrix = tableToArray table'
    ((0,0),(n,m)) = bounds rawMatrix
    emptyEntry = Cell [] 0 0

    updateVisited :: Position -> State (Visited, Matrix Cell) ()
    updateVisited (i, j) = do
        (visited, mat) <- get
        let newVisited (x,y) = (y == i && x == j) || visited (x,y)
        put (newVisited, mat)

    updateMatrix :: Position -> Cell -> State (Visited, Matrix Cell) ()
    updateMatrix pos val = do
        (visited, mat) <- get
        let newMat = mat // [(pos, val)]
        put (visited, newMat)

    maxWidth (i, j) = (1+) $ length $ takeWhile (\x -> x <= m && rawMatrix ! (i,x) == MergeLeft) [j+1..m] 

    maxHeight (i, j) w =
        (1+) $ length $ takeWhile (\y -> y <= n && rawMatrix ! (y, j) == MergeUp 
                                        && all (\x -> let val = rawMatrix ! (y,x)
                                                      in val == MergeLeft || val == MergeUp) [j+1..j+w-1]) [i+1..n]

    process =
        forM_ [0..n] $ \i ->
          forM_ [0..m] $ \j -> do
            let pos = (i, j)
            (visited, _) <- get
            if visited pos
                then updateMatrix pos emptyEntry
                else do
                    case rawMatrix ! pos of
                      EmptyCell -> do
                          updateVisited pos
                          updateMatrix pos emptyEntry
                      MergeLeft -> do
                          updateVisited pos
                          updateMatrix pos (Cell [Word "<"] 1 1)
                      MergeUp -> do
                          updateVisited pos
                          updateMatrix pos (Cell [Word "^"] 1 1)
                      Cell' content -> do
                        let w = maxWidth pos
                        let h = maxHeight pos w

                        forM_ [i..i+h-1] $ \y ->
                            forM_ [j..j+w-1] $ \x ->
                                updateVisited (x, y)

                        updateMatrix pos (Cell content w h)

-- Convert a list of lists to an array
tableToArray :: Table' -> Matrix Cell'
tableToArray t =
    let n = length (unTable' t) - 1
        m = if n < 0 then 0 else length (unRow' (head (unTable' t))) - 1
    in
    array ((0,0), (n,m))
            [ ((i,j), getCell i j) | i <- [0..n], j <- [0..m] ]
    where
    rows = unTable' t
    getCell i j = unRow' (rows !! i) !! j

-- Convert array back to list of lists (optional)
arrayToTable :: Matrix Cell -> Table
arrayToTable arr =
    let ((i0,j0),(in_,jn)) = bounds arr
    in Table [ Row [ arr ! (i,j) | j <- [j0..jn]] | i <- [i0..in_]]

-- pad rows to equal length
padTable :: Table' -> Table'
padTable (Table' rows) =
    let maxLen = maximum (map rowLen rows)
    in Table' (map (padRow maxLen) rows)
    where
    rowLen (Row' cs) = length cs
    padRow n (Row' cs) =
        Row' (cs ++ replicate (n - length cs) EmptyCell)