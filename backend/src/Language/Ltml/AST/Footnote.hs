module Language.Ltml.AST.Footnote
    ( Footnote (..)
    )
where

import Language.Lsd.AST.Type.Footnote (FootnoteFormat)
import Language.Ltml.AST.Text (FootnoteTextTree)

data Footnote
    = Footnote
        FootnoteFormat
        [FootnoteTextTree]
    deriving (Show)
