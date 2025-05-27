module Language.Lsd.AST.Paragraph
    ( ParagraphFormat
    )
where

import Language.Lsd.AST.Format (IdentifierFormat)

newtype ParagraphFormat
    = ParagraphFormat
        IdentifierFormat
