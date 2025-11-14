module Language.Ltml.AST.Table
    ( Table (..)
    , Row (..)
    , Cell (..)
    )
where

import Language.Ltml.AST.Text (TableTextTree)
import Language.Lsd.AST.Type.Table (CellFormat)

-- the internal representation of a table
newtype Table = Table [Row]

instance Show Table where
    show (Table rows) = unlines $ map show rows

newtype Row = Row [Cell]
    deriving (Show)

data Cell = Cell CellFormat [TableTextTree] Int Int 
          | SpannedCell -- for merged cells. equivalent to Cell _ [] 0 0, 
                        -- but since they dont need a format, we use a separate constructor
    deriving (Show)
