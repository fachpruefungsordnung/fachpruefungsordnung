{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.AST.Text
    ( TextTree (..)
    , PlainTextTree
    , FootnoteTextTree
    , RichTextTree
    , ParagraphTextTree
    , FontStyle (..)
    , Enumeration (..)
    , EnumItem (..)
    , SentenceStart (..)
    )
where

import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Data.Void (Void)
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Node (Node)

data TextTree style enum special
    = Word Text
    | Space
    | Special special
    | Reference Label
    | Styled style [TextTree style enum special]
    | Enum enum
    | Footnote [FootnoteTextTree]
    deriving (Show)

instance FromWhitespace [TextTree a b c] where
    fromWhitespace "" = []
    fromWhitespace _ = [Space]

type PlainTextTree = TextTree Void Void Void

type FootnoteTextTree = TextTree FontStyle Void Void

type RichTextTree = TextTree FontStyle Enumeration Void

type ParagraphTextTree = TextTree FontStyle Enumeration SentenceStart

data FontStyle
    = Bold
    | Italics
    | Underlined
    deriving (Show)

newtype Enumeration = Enumeration [Node EnumItem]
    deriving (Show)

newtype EnumItem = EnumItem [RichTextTree]
    deriving (Show)

newtype SentenceStart = SentenceStart (Maybe Label)
    deriving (Show)
