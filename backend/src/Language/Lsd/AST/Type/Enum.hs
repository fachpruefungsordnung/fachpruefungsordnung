module Language.Lsd.AST.Type.Enum
    ( EnumFormat (..)
    , EnumItemFormat (..)
    , EnumType (..)
    , PreEnumType (..)
    )
where

import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.Format (EnumItemKeyFormat, IdentifierFormat)
import Language.Lsd.AST.Type.Text
    ( PreTextType
    , TextType
    )

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

data PreEnumType
    = PreEnumType
        Keyword
        EnumFormat
        (PreTextType TypeName)
