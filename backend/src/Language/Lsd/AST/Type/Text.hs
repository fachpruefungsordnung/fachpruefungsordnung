module Language.Lsd.AST.Type.Text
    ( TextType (..)
    , PreTextType (..)
    , FootnoteType (..)
    , PreFootnoteType (..)
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)

data TextType enumT
    = TextType
        [enumT]
        [FootnoteType]

data PreTextType enumT
    = PreTextType
        [enumT]
        [PreFootnoteType]

data FootnoteType
    = FootnoteType
        Keyword
        (TextType Void)

data PreFootnoteType
    = PreFootnoteType
        Keyword
        (PreTextType Void)
