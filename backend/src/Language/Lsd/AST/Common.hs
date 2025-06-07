module Language.Lsd.AST.Common
    ( TypeName (..)
    , Keyword (..)
    )
where

import Data.Text (Text)

newtype TypeName = TypeName String

newtype Keyword = Keyword Text
