module Language.Lsd.AST.Type.SimpleParagraph
    ( SimpleParagraphFormat (..)
    , SimpleParagraphType (..)
    )
where

import Data.Typography (Typography)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Lsd.AST.Type.Text (TextType)

newtype SimpleParagraphFormat
    = SimpleParagraphFormat
        Typography
    deriving (Show)

data SimpleParagraphType
    = SimpleParagraphType
        SimpleParagraphFormat
        (TextType EnumType)
