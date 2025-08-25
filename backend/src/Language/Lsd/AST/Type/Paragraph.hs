module Language.Lsd.AST.Type.Paragraph
    ( ParagraphFormat (..)
    , ParagraphType (..)
    )
where

import Language.Lsd.AST.Format (IdentifierFormat, ParagraphKeyFormat)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Lsd.AST.Type.Text (TextType)

data ParagraphFormat
    = ParagraphFormat
        IdentifierFormat
        ParagraphKeyFormat
    deriving (Show)

data ParagraphType
    = ParagraphType
        ParagraphFormat
        (TextType EnumType)
