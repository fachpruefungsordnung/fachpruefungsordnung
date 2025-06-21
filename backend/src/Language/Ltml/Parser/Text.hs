{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.Parser.Text
    ( textForestP
    , hangingTextP'
    )
where

import Control.Applicative (empty, (<|>))
import Control.Applicative.Combinators (choice)
import Control.Monad (void)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans.Class (lift)
import qualified Data.Char as Char (isControl)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text (singleton)
import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Enum (EnumType (EnumType))
import Language.Lsd.AST.Type.Text
    ( FootnoteType (FootnoteType)
    , TextType (TextType)
    )
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Text
    ( EnumItem (EnumItem)
    , FontStyle (..)
    , FootnoteTextTree
    , SentenceStart (SentenceStart)
    , TextTree (..)
    )
import Language.Ltml.Parser
    ( MonadParser
    , Parser
    , ParserWrapper (wrapParser)
    )
import Language.Ltml.Parser.Keyword (keywordP, mlKeywordP)
import Language.Ltml.Parser.Label (labelP, labelingP)
import Language.Ltml.Parser.MiTree (hangingBlock', hangingBlock_, miForest)
import Text.Megaparsec
    ( eof
    , notFollowedBy
    , satisfy
    , some
    , takeWhile1P
    , try
    )
import Text.Megaparsec.Char (char)

type ParagraphParser =
    StateT
        Bool -- whether sentence start is expected
        Parser

instance ParserWrapper ParagraphParser where
    wrapParser = lift

textForestP
    :: ( MonadParser m
       , StyleP style
       , EnumP enumType enumItem
       , SpecialP m special
       )
    => TextType enumType
    -> m [TextTree style enumItem special]
textForestP t = miForest elementPF (childPF t)

elementPF
    :: forall m style enumItem special
     . (MonadParser m, StyleP style, SpecialP m special)
    => m [TextTree style enumItem special]
    -> m (TextTree style enumItem special)
elementPF p =
    Special <$> specialP
        <|> Word <$> wordP (Proxy :: Proxy special)
        <|> Reference <$ char '{' <* char ':' <*> labelP <* char '}'
        <|> Styled <$ char '<' <*> styleP <*> p <* char '>'

childPF
    :: (ParserWrapper m, EnumP enumType enumItem)
    => TextType enumType
    -> m (TextTree style enumItem special)
childPF (TextType enumTypes footnoteTypes) =
    wrapParser $
        EnumChild <$> choice (fmap enumItemP enumTypes)
            <|> Footnote <$> choice (fmap footnoteTextP footnoteTypes)

footnoteTextP :: FootnoteType -> Parser [FootnoteTextTree]
footnoteTextP (FootnoteType kw tt) = hangingTextP kw tt

hangingTextP
    :: ( MonadParser m
       , StyleP style
       , EnumP enumType enumItem
       , SpecialP m special
       )
    => Keyword
    -> TextType enumType
    -> m [TextTree style enumItem special]
hangingTextP kw t = hangingBlock_ (keywordP kw) elementPF (childPF t)

hangingTextP'
    :: ( MonadParser m
       , StyleP style
       , EnumP enumType enumItem
       , SpecialP m special
       )
    => Keyword
    -> TextType enumType
    -> m (Maybe Label, [TextTree style enumItem special])
hangingTextP' kw t = hangingBlock' (mlKeywordP kw) elementPF (childPF t)

class StyleP style where
    styleP :: (MonadParser m) => m style

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

class (ParserWrapper m) => SpecialP m special | special -> m where
    specialP :: m special
    wordP :: Proxy special -> m Text

instance SpecialP Parser Void where
    specialP = empty

    wordP _ = gWordP isWordChar isWordSpecialChar

instance SpecialP ParagraphParser SentenceStart where
    specialP = do
        isSentenceStartExpected <- get
        if isSentenceStartExpected
            then (labeledSSP <|> unlabeledSSP) <* put False
            else empty
      where
        -- Labeled sentence start tokens (SSTs) may occur whenever the state
        -- permits, while unlabeled SSPs are not permitted at the end of a
        -- paragraph (followed by `\n` or EOF), or before styling tags
        -- (`<`, `>`).
        --  - For styling tags, parsing an unlabeled SST is delayed until
        --    after them, while a labaled SST may occur either before or after
        --    a styling tag.
        --  - Labeled SSTs should also be disallowed at the end of a
        --    paragraph, but that is non-trivial to catch during parsing, and
        --    probably better done later (TODO).
        --    - TODO: Also consider the case where that SST is the paragraph's
        --      only token.

        -- TODO: Avoid `try`.
        labeledSSP :: ParagraphParser SentenceStart
        labeledSSP = SentenceStart . Just <$> try labelingP

        unlabeledSSP :: ParagraphParser SentenceStart
        unlabeledSSP =
            SentenceStart Nothing <$ notFollowedBy specialSuccP
          where
            specialSuccP :: ParagraphParser ()
            specialSuccP = void (satisfy isSpecialSuccChar) <|> eof
              where
                isSpecialSuccChar :: Char -> Bool
                isSpecialSuccChar '\n' = True
                isSpecialSuccChar '<' = True
                isSpecialSuccChar '>' = True
                isSpecialSuccChar _ = False

    wordP _ = sentenceWordP <|> sentenceEndP
      where
        sentenceWordP :: ParagraphParser Text
        sentenceWordP = gWordP isWordChar isSentenceSpecialChar

        sentenceEndP :: ParagraphParser Text
        sentenceEndP = Text.singleton <$> satisfy isSentenceEndChar <* put True

gWordP :: (MonadParser m) => (Char -> Bool) -> (Char -> Bool) -> m Text
gWordP isValid isSpecial = mconcat <$> some (regularWordP <|> escapedCharP)
  where
    regularWordP :: (MonadParser m) => m Text
    regularWordP = takeWhile1P Nothing isRegular
      where
        isRegular :: Char -> Bool
        isRegular c = isValid c && not (isSpecial c)

    escapedCharP :: (MonadParser m) => m Text
    escapedCharP = Text.singleton <$ char '\\' <*> satisfy isValid

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

isSentenceEndChar :: Char -> Bool
isSentenceEndChar '.' = True
isSentenceEndChar '!' = True
isSentenceEndChar '?' = True
isSentenceEndChar _ = False

isSentenceSpecialChar :: Char -> Bool
isSentenceSpecialChar c = isWordSpecialChar c || isSentenceEndChar c
