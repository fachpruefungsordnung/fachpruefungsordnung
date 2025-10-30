module Language.Lsd.AST.Type.Table
    ( TableType (..)
    , RowType (..)
    , CellType (..)
    )
where

import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Star)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Lsd.AST.Type.Text (TextType)

data TableType = TableType Keyword (Star RowType)

newtype RowType = RowType (Star CellType)

newtype CellType = CellType (TextType EnumType)
