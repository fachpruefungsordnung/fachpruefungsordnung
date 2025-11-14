module Language.Lsd.AST.Type.Table
    ( TableType (..)
    , RowType (..)
    , DefaultCellType (..)
    , CellType (..)
    , CellFormat (..)
    , BGColor (..)
    )
where

import Data.Typography (Typography)
import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Disjunction, Star)
import Language.Lsd.AST.Type.Text (TextType)

data TableType
    = TableType
        Keyword -- this defines the "main" keyword for the table. e.g. "|"
        DefaultCellType -- this defines the default cell type for the table
        (Star RowType)

data RowType
    = RowType
        Keyword -- this defines the keyword that ends the row, e.g. "&"
        -- together with the main table keyword, this defines the parsing of the row
        -- e.g. if the main table keyword is "|" and the row ending keyword is "&",
        -- then the end of a row is marked with "|&"
        (Star (Disjunction CellType))

data BGColor = White | Gray
    deriving (Show)

data CellFormat
    = CellFormat
        BGColor
        Typography
    deriving (Show)

newtype DefaultCellType = DefaultCellType CellType

data CellType
    = CellType
        Keyword -- this defines the keyword that specifies the cell. e.g. "*" or "-"
        -- together with the main table keyword, this defines the parsing of the cell
        -- e.g. if the main table keyword is "|" and the cell keyword is "*",
        -- then the start of the cell is marked with "|*"
        CellFormat
        (TextType Void)
