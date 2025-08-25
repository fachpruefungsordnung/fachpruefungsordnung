{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.AST.Text
    ( TextTree (..)
    , HeadingTextTree
    , FootnoteTextTree
    , RichTextTree
    , ParagraphTextTree
    , HardLineBreak (..)
    , Enumeration (..)
    , EnumItem (..)
    , SentenceStart (..)
    , FootnoteReference (..)
    )
where

import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Data.Typography (FontStyle)
import Data.Void (Void)
import Language.Lsd.AST.Type.Enum (EnumFormat)
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Node (Node)

data TextTree lbrk fnref style enum special
    = Word Text
    | Space
    | NonBreakingSpace
    | LineBreak lbrk
    | Special special
    | Reference Label
    | Styled style [TextTree lbrk fnref style enum special]
    | Enum enum
    | FootnoteRef fnref
    deriving (Show)

instance FromWhitespace [TextTree a b c d e] where
    fromWhitespace "" = []
    fromWhitespace _ = [Space]

type HeadingTextTree = TextTree Void FootnoteReference Void Void Void

type FootnoteTextTree = TextTree HardLineBreak Void FontStyle Void Void

type RichTextTree =
    TextTree HardLineBreak FootnoteReference FontStyle Enumeration Void

type ParagraphTextTree =
    TextTree
        HardLineBreak
        FootnoteReference
        FontStyle
        Enumeration
        SentenceStart

data HardLineBreak = HardLineBreak
    deriving (Show)

data Enumeration = Enumeration EnumFormat [Node EnumItem]
    deriving (Show)

newtype EnumItem = EnumItem [RichTextTree]
    deriving (Show)

newtype SentenceStart = SentenceStart (Maybe Label)
    deriving (Show)

newtype FootnoteReference = FootnoteReference Label
    deriving (Show)
