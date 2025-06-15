module Language.Ltml.AST.Paragraph
    ( Paragraph (..)
    )
where

import Language.Lsd.AST.Type.Paragraph (ParagraphFormat)
import Language.Ltml.AST.Text (ParagraphTextTree)

data Paragraph
    = Paragraph
        ParagraphFormat
        [ParagraphTextTree]
    deriving (Show)
