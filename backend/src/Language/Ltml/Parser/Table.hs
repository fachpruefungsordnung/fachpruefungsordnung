{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Table
    ( tableP
    )
where

import Control.Monad (forM_)
import Control.Monad.State (State, execState, get, put)
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
    ( MonadParsec (eof, hidden, takeWhileP, try)
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
    rows <- nLexeme $ some (rowP t)
    hidden space
    eof
    pure (Table' rows)

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
    let matrix' = array ((0, 0), (n, m)) [((i, j), emptyEntry) | i <- [0 .. n], j <- [0 .. m]]
        visited0 (_, _) = False
     in arrayToTable $ snd $ execState process (visited0, matrix')
  where
    table' = padTable table
    rawMatrix = tableToArray table'
    ((0, 0), (n, m)) = bounds rawMatrix
    emptyEntry = Cell [] 0 0

    updateVisited :: Position -> State (Visited, Matrix Cell) ()
    updateVisited (i, j) = do
        (visited, mat) <- get
        let newVisited (x, y) = (y == i && x == j) || visited (x, y)
        put (newVisited, mat)

    updateMatrix :: Position -> Cell -> State (Visited, Matrix Cell) ()
    updateMatrix pos val = do
        (visited, mat) <- get
        let newMat = mat // [(pos, val)]
        put (visited, newMat)

    maxWidth (i, j) =
        (1 +) $
            length $
                takeWhile (\x -> x <= m && rawMatrix ! (i, x) == MergeLeft) [j + 1 .. m]

    maxHeight (i, j) w =
        (1 +) $
            length $
                takeWhile
                    ( \y ->
                        y <= n
                            && rawMatrix ! (y, j) == MergeUp
                            && all
                                ( \x ->
                                    let val = rawMatrix ! (y, x)
                                     in val == MergeLeft || val == MergeUp
                                )
                                [j + 1 .. j + w - 1]
                    )
                    [i + 1 .. n]

    process =
        forM_ [0 .. n] $ \i ->
            forM_ [0 .. m] $ \j -> do
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

                                forM_ [i .. i + h - 1] $ \y ->
                                    forM_ [j .. j + w - 1] $ \x ->
                                        updateVisited (x, y)

                                updateMatrix pos (Cell content w h)

-- Convert a list of lists to an array
tableToArray :: Table' -> Matrix Cell'
tableToArray t =
    let n = length (unTable' t) - 1
        m = if n < 0 then 0 else length (unRow' (head (unTable' t))) - 1
     in array
            ((0, 0), (n, m))
            [((i, j), getCell i j) | i <- [0 .. n], j <- [0 .. m]]
  where
    rows = unTable' t
    getCell i j = unRow' (rows !! i) !! j

-- Convert array back to list of lists (optional)
arrayToTable :: Matrix Cell -> Table
arrayToTable arr =
    let ((i0, j0), (in_, jn)) = bounds arr
     in Table [Row [arr ! (i, j) | j <- [j0 .. jn]] | i <- [i0 .. in_]]

-- pad rows to equal length
padTable :: Table' -> Table'
padTable (Table' rows) =
    let maxLen = maximum (map rowLen rows)
     in Table' (map (padRow maxLen) rows)
  where
    rowLen (Row' cs) = length cs
    padRow n (Row' cs) =
        Row' (cs ++ replicate (n - length cs) EmptyCell)
