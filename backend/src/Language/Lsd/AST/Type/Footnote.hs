module Language.Lsd.AST.Type.Footnote
    ( FootnoteFormat (..)
    , FootnoteType (..)
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Text (TextType)

data FootnoteFormat = SuperscriptFootnoteFormat
    deriving (Show)

data FootnoteType
    = FootnoteType
        Keyword
        FootnoteFormat
        (TextType Void)
