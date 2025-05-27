{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.AST.Paragraph (Paragraph) where

import Control.Applicative ((<|>))
import qualified Data.Char as Char (isAlpha)
import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Language.Lsd.AST.Paragraph (ParagraphFormat)
import Language.Ltml.AST.Label (Label)
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.MiTree (hangingBlock', miForest)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (string)

data Paragraph
    = Paragraph
        ParagraphFormat
        [RichTextTree]

data RichTextTree
    = TextLeaf Text
    | SentenceStart (Maybe Label)
    | Reference Text
    | Styled FontStyle [RichTextTree]
    | EnumItem [RichTextTree]
    | Footnote Text
    deriving (Show)

data FontStyle
    = Bold
    | Italics
    | Underlined
    deriving (Show)

instance FromWhitespace RichTextTree where
    fromWhitespace "" = TextLeaf ""
    fromWhitespace _ = TextLeaf " "

paragraphP :: ParagraphFormat -> Parser Paragraph
paragraphP pf = Paragraph pf <$> richTextForestP

richTextForestP :: Parser [RichTextTree]
richTextForestP = miForest elementPF childP
  where
    elementPF :: Parser [RichTextTree] -> Parser RichTextTree
    elementPF p =
        TextLeaf <$> takeWhile1P (Just "word") Char.isAlpha {- TODO -}
            <|> Styled Bold <$> (string "<*" *> p <* string "*>")
            <|> Styled Italics <$> (string "</" *> p <* string "/>")
            <|> Styled Underlined <$> (string "<_" *> p <* string "_>")

    childP :: Parser RichTextTree
    childP = EnumItem <$> hangingBlock' "#" elementPF childP
