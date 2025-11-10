{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Table
    ( tableP
    )
where

import Control.Monad (forM_)
import Control.Monad.State (State, execState, get, modify)
import Data.Array (Array, array, bounds, (!), (//))
import Data.Text (pack)
import qualified Data.Text as T
import Language.Lsd.AST.SimpleRegex (Star (Star))
import Language.Lsd.AST.Type.Table
    ( CellType (CellType)
    , RowType (RowType)
    , TableType (TableType)
    )
import Language.Ltml.AST.Table
    ( Cell (..)
    , Row (..)
    , Table (..)
    )
import Language.Ltml.AST.Text (TableTextTree, TextTree (Word))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Text (textForestP)
import Text.Megaparsec
    ( MonadParsec ( hidden, takeWhileP, try)
    , errorBundlePretty
    , manyTill
    , runParser
    , some
    )
import Text.Megaparsec.Char (char, space, string)

cellP :: CellType -> Parser Cell'
cellP (CellType tt) = do
    _ <- char '|'
    space
    -- Take everything until we *see* a table boundary
    chunk <- takeWhileP (Just "cell text") (`notElem` ['|'])
    if
        | T.null chunk -> pure EmptyCell
        | T.strip chunk == "<" -> pure MergeLeft
        | T.strip chunk == "^" -> pure MergeUp
        | otherwise ->
            case runParser (textForestP tt) "" (chunk <> "\n") of
                Left err -> pure (Cell' [Word $ pack $ errorBundlePretty err])
                Right forest -> pure (Cell' forest)

-- parse a row ending with |&
rowP :: RowType -> Parser Row'
rowP (RowType (Star t)) = do
    cells <- nLexeme $ manyTill (cellP t) (try (string "|&"))
    pure (Row' cells)

-- parse an entire table
tableP' :: TableType -> Parser Table'
tableP' (TableType _ (Star t)) = do
    space
    nRows <- nLexeme $ some (rowP t)
    hidden space
    _ <- char '&'
    pure (Table' nRows)

tableP :: TableType -> Parser Table
tableP tt = do
    tbl' <- tableP' tt
    pure (mergeCells tbl')

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
    let matrix' = array ((0, 0), (nRows, mCols)) [((row, col), emptyEntry) | row <- [0 .. nRows], col <- [0 .. mCols]]
        visited0 (_, _) = False
     in arrayToTable $ snd $ process (0, 0) (visited0, matrix')
  where
    table' = padTable table
    rawMatrix = tableToArray table'
    ((0, 0), (nRows, mCols)) = bounds rawMatrix
    emptyEntry = Cell [] 0 0

    maxWidth :: Position -> Int
    maxWidth (row, col) =
        (1 +) $
            length $
                takeWhile (\col' -> col' <= mCols && rawMatrix ! (row, col') == MergeLeft) [col + 1 .. mCols]

    maxHeight :: Position -> Int -> Int
    maxHeight (row, col) w =
        (1 +) $
            length $
                takeWhile
                    ( \row' ->
                        row' <= nRows
                            && rawMatrix ! (row', col) == MergeUp
                            && all
                                ( \col' ->
                                    let val = rawMatrix ! (row', col')
                                     in val == MergeLeft || val == MergeUp
                                )
                                [col + 1 .. col + w - 1]
                    )
                    [row + 1 .. nRows]
        
    updateVisited :: Position -> (Visited, Matrix Cell) -> (Visited, Matrix Cell)
    updateVisited p0 (visited, mat) = (\p -> p == p0 || visited p, mat )

    updateMatrix :: Position -> Cell -> (Visited, Matrix Cell) -> (Visited, Matrix Cell)
    updateMatrix pos val (visited, mat) = (visited, mat // [(pos, val)])

    process :: Position -> (Visited, Matrix Cell) -> (Visited, Matrix Cell)
    process (row, col) s@(visited, _) 
        | row > nRows = s 
        | col > mCols = process (row+1, 0) s
        | visited (row, col) = process (row, col + 1) s
        | otherwise =
            let createCell content =
                    let w = maxWidth (row, col)
                        h = maxHeight (row, col) w
                        s' = foldl
                                (\acc (x, y) -> updateVisited (x, y) acc)
                                s
                                [ (row', col')
                                | col' <- [col .. col + w - 1]
                                , row' <- [row .. row + h - 1]
                                ]
                     in updateMatrix (row, col) (Cell content w h) s'
                res = case rawMatrix ! (row, col) of
                        EmptyCell -> createCell []
                        MergeLeft -> createCell [Word "<"]
                        MergeUp -> createCell [Word "^"]
                        Cell' content -> createCell content
             in process (row, col+1) res

-- Convert a list of lists to an array
tableToArray :: Table' -> Matrix Cell'
tableToArray t =
    let nRows = length (unTable' t) - 1
        mCols = if nRows < 0 then 0 else length (unRow' (head (unTable' t))) - 1
     in array
            ((0, 0), (nRows, mCols))
            [((row, col), getCell row col) | row <- [0 .. nRows], col <- [0 .. mCols]]
  where
    nRows = unTable' t
    getCell row col = unRow' (nRows !! row) !! col

-- Convert array back to list of lists (optional)
arrayToTable :: Matrix Cell -> Table
arrayToTable arr =
    let ((i0, j0), (in_, jn)) = bounds arr
     in Table [Row [arr ! (row, col) | col <- [j0 .. jn]] | row <- [i0 .. in_]]

-- pad nRows to equal length
padTable :: Table' -> Table'
padTable (Table' rows) =
    let maxLen = maximum (map rowLen rows)
     in Table' (map (padRow maxLen) rows)
  where
    rowLen (Row' cs) = length cs
    padRow rows (Row' cs) =
        Row' (cs ++ replicate (rows - length cs) EmptyCell)


-- first attempt using State monad (commented out)
    -- updateVisited :: Position -> State (Visited, Matrix Cell) ()
    -- updateVisited p0 = modify $ \(visited, mat) -> (\p -> p == p0 || visited p, mat )

    -- updateMatrix :: Position -> Cell -> State (Visited, Matrix Cell) ()
    -- updateMatrix pos val = modify $ \(visited, mat) -> (visited, mat // [(pos, val)])

    -- process =
    --     forM_ [0 .. nRows] $ \row ->
    --         forM_ [0 .. mCols] $ \col -> do
    --             let pos = (row, col)
    --             (visited, _) <- get
    --             if visited pos
    --                 then updateMatrix pos emptyEntry
    --                 else do
    --                     let createCell content = do
    --                             let w = maxWidth pos
    --                             let h = maxHeight pos w

    --                             forM_ [row .. row + h - 1] $ \y ->
    --                                 forM_ [col .. col + w - 1] $ \x ->
    --                                     updateVisited (x, y)

    --                             updateMatrix pos (Cell content w h)
    --                     case rawMatrix ! pos of
    --                         EmptyCell -> createCell []
    --                         MergeLeft -> createCell [Word "<"]
    --                         MergeUp -> createCell [Word "^"]
    --                         Cell' content -> createCell content