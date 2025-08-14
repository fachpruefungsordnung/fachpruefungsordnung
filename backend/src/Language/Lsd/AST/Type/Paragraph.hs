module Language.Lsd.AST.Type.Paragraph
    ( ParagraphFormat (..)
    , ParagraphType (..)
    , PreParagraphType (..)
    )
where

import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.Format (IdentifierFormat, ParagraphKeyFormat)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Lsd.AST.Type.Text
    ( PreTextType
    , TextType
    )

data ParagraphFormat
    = ParagraphFormat
        IdentifierFormat
        ParagraphKeyFormat
    deriving (Show)

data ParagraphType
    = ParagraphType
        ParagraphFormat
        (TextType EnumType)

data PreParagraphType
    = PreParagraphType
        ParagraphFormat
        (PreTextType TypeName)
