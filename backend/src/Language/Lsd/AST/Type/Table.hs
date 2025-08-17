module Language.Lsd.AST.Type.Table
    ( TableType (..)
    , PreTableType (..)
    )
where

import Language.Lsd.AST.Common (Keyword)

newtype TableType = TableType Keyword

newtype PreTableType = PreTableType Keyword
