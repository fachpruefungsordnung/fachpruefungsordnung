module Language.Lsd.AST.Type.Enum
    ( EnumFormat (..)
    , EnumItemFormat (..)
    , EnumType (..)
    )
where

import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Format (EnumItemKeyFormat, IdentifierFormat)
import Language.Lsd.AST.Type.Text (TextType)

newtype EnumFormat = EnumFormat EnumItemFormat
    deriving (Show, Eq)

data EnumItemFormat
    = EnumItemFormat
        IdentifierFormat
        EnumItemKeyFormat
    deriving (Show, Eq)

data EnumType
    = EnumType
        Keyword
        EnumFormat
        (TextType EnumType)
