module Language.Ltml.AST.SimpleParagraph
    ( SimpleParagraph (..)
    )
where

import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphFormat)
import Language.Ltml.AST.Text (RichTextTree)

data SimpleParagraph
    = SimpleParagraph
        SimpleParagraphFormat
        [RichTextTree]
    deriving (Show)
