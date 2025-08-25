module Language.Lsd.AST.Type.Table
    ( TableType (..)
    )
where

import Language.Lsd.AST.Common (Keyword)

newtype TableType = TableType Keyword
