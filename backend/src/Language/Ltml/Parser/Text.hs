{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Ltml.Parser.Text
    ( textForestP
    , hangingTextP
    )
where

import Control.Applicative (empty, (<|>))
import Control.Applicative.Combinators (choice)
import qualified Data.Char as Char (isControl)
import Data.Text (Text)
import qualified Data.Text as Text (singleton)
import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword (..))
import Language.Lsd.AST.Type.Enum (EnumType (..))
import Language.Lsd.AST.Type.Text
    ( FootnoteType (..)
    , TextType (..)
    )
import Language.Ltml.AST.Text
    ( EnumItem (..)
    , FontStyle (..)
    , FootnoteTextTree
    , SentenceStart (..)
    , TextTree (..)
    )
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Label (labelP)
import Language.Ltml.Parser.MiTree (hangingBlock', miForest)
import Text.Megaparsec (satisfy, some, takeWhile1P)
import Text.Megaparsec.Char (char)

textForestP
    :: (StyleP style, EnumP enumType enumItem, SpecialP special)
    => TextType enumType
    -> Parser [TextTree style enumItem special]
textForestP t = miForest elementPF (childPF t)

elementPF
    :: (StyleP style, SpecialP special)
    => Parser [TextTree style enumItem special]
    -> Parser (TextTree style enumItem special)
elementPF p =
    Special <$> specialP
        <|> textLeafP
        <|> Reference <$ char '{' <*> labelP <* char '}'
        <|> Styled <$ char '<' <*> styleP <*> p <* char '>'

childPF
    :: (EnumP enumType enumItem)
    => TextType enumType
    -> Parser (TextTree style enumItem special)
childPF (TextType enumTypes footnoteTypes) =
    EnumChild <$> choice (fmap enumItemP enumTypes)
        <|> Footnote <$> choice (fmap footnoteTextP footnoteTypes)

footnoteTextP :: FootnoteType -> Parser [FootnoteTextTree]
footnoteTextP (FootnoteType kw tt) = hangingTextP kw tt

hangingTextP
    :: (StyleP style, EnumP enumType enumItem, SpecialP special)
    => Keyword
    -> TextType enumType
    -> Parser [TextTree style enumItem special]
hangingTextP (Keyword kw) t = hangingBlock' kw elementPF (childPF t)

class StyleP style where
    styleP :: Parser style

instance StyleP Void where
    styleP = empty

instance StyleP FontStyle where
    styleP =
        Bold <$ char '*'
            <|> Italics <$ char '/'
            <|> Underlined <$ char '_'

class EnumP enumType enumItem where
    enumItemP :: enumType -> Parser enumItem

instance EnumP Void Void where
    enumItemP = const empty

instance EnumP EnumType EnumItem where
    enumItemP (EnumType kw tt) = EnumItem <$> hangingTextP kw tt

class SpecialP special where
    specialP :: Parser special
    textLeafP :: Parser (TextTree a b special)

instance SpecialP Void where
    specialP = empty
    textLeafP = TextLeaf <$> simpleWordP

instance SpecialP SentenceStart where
    specialP = pure $ SentenceStart Nothing -- TODO
    textLeafP = TextLeaf <$> simpleWordP -- TODO

simpleWordP :: Parser Text
simpleWordP =
    mconcat
        <$> some
            ( takeWhile1P Nothing isWordRegularChar
                <|> Text.singleton <$ char '\\' <*> satisfy isWordChar
            )

-- NOTE: isControl '\n' == True
isWordChar :: Char -> Bool
isWordChar ' ' = False
isWordChar c = not $ Char.isControl c

isWordSpecialChar :: Char -> Bool
isWordSpecialChar '\\' = True
isWordSpecialChar '{' = True
isWordSpecialChar '}' = True
isWordSpecialChar '<' = True
isWordSpecialChar '>' = True
isWordSpecialChar _ = False

isWordRegularChar :: Char -> Bool
isWordRegularChar c = isWordChar c && not (isWordSpecialChar c)
