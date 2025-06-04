module Language.Ltml.AST.Paragraph
    ( Paragraph (..)
    )
where

import Language.Lsd.AST.Type.Paragraph (ParagraphFormat)
import Language.Ltml.AST.Text (RichTextTree)

data Paragraph
    = Paragraph
        ParagraphFormat
        [RichTextTree]
    deriving (Show)
