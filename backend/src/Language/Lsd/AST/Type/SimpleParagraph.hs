module Language.Lsd.AST.Type.SimpleParagraph
    ( SimpleParagraphFormat (..)
    , SimpleParagraphType (..)
    , PreSimpleParagraphType (..)
    )
where

import Data.Typography (Typography)
import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Lsd.AST.Type.Text
    ( PreTextType
    , TextType
    )

newtype SimpleParagraphFormat
    = SimpleParagraphFormat
        Typography
    deriving (Show)

data SimpleParagraphType
    = SimpleParagraphType
        SimpleParagraphFormat
        (TextType EnumType)

data PreSimpleParagraphType
    = PreSimpleParagraphType
        SimpleParagraphFormat
        (PreTextType TypeName)
