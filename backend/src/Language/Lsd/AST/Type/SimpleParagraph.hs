module Language.Lsd.AST.Type.SimpleParagraph
    ( SimpleParagraphFormat (..)
    , SimpleParagraphType (..)
    , PreSimpleParagraphType (..)
    )
where

import Data.Typography (FontSize, TextAlignment)
import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Lsd.AST.Type.Text
    ( PreTextType
    , TextType
    )

data SimpleParagraphFormat
    = SimpleParagraphFormat
        TextAlignment
        FontSize
    deriving (Show)

data SimpleParagraphType
    = SimpleParagraphType
        SimpleParagraphFormat
        (TextType EnumType)

data PreSimpleParagraphType
    = PreSimpleParagraphType
        SimpleParagraphFormat
        (PreTextType TypeName)
