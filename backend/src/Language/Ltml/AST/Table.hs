module Language.Ltml.AST.Table
    ( Table (..)
    , Row (..)
    , Cell (..)
    )
where

import Language.Lsd.AST.Type.Table (CellFormat)
import Language.Ltml.AST.Text (TableTextTree)

-- the internal representation of a table
type ColumnProps = Maybe [Int]
data Table = Table ColumnProps [Row]

instance Show Table where
    show (Table _ rows) = unlines $ map show rows

newtype Row = Row [Cell]
    deriving (Show)

type Width = Int
type Height = Int

data Cell
    = Cell CellFormat [TableTextTree] Width Height
    | VSpannedCell Int
    | HSpannedCell -- for merged cells. equivalent to Cell _ [] 0 0,
    -- but since they dont need a format, we use a separate constructor
    deriving (Show)

instance Eq Cell where
    (Cell {}) == (Cell {}) = True
    VSpannedCell {} == VSpannedCell {} = True
    HSpannedCell {} == HSpannedCell {} = True
    _ == _ = False
