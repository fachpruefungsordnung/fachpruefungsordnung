{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.Parser.Text
    ( textForestP
    , hangingTextP'
    )
where

import Control.Applicative (empty, (<|>))
import Control.Applicative.Combinators (choice)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans.Class (lift)
import qualified Data.Char as Char (isControl)
import Data.List (singleton)
import Data.Maybe (maybeToList)
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
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Text
    ( EnumItem (EnumItem)
    , Enumeration (Enumeration)
    , FontStyle (..)
    , FootnoteTextTree
    , SentenceStart (SentenceStart)
    , TextTree (..)
    )
import Language.Ltml.Parser
    ( MonadParser
    , Parser
    , ParserWrapper (wrapParser)
    , someIndented
    )
import Language.Ltml.Parser.Keyword (keywordP, mlKeywordP)
import Language.Ltml.Parser.Label (labelP, labelingP)
import Language.Ltml.Parser.MiTree
    ( MiElementConfig (..)
    , hangingBlock'
    , hangingBlock_
    , miForest
    )
import Text.Megaparsec
    ( notFollowedBy
    , satisfy
    , some
    , takeWhile1P
    , try
    )
import Text.Megaparsec.Char (char, string)

type ParagraphParser =
    StateT
        Bool -- whether sentence start is expected
        Parser

instance ParserWrapper ParagraphParser where
    wrapParser = lift

textForestP
    :: ( ParserWrapper m
       , StyleP style
       , EnumP enumType enum
       , SpecialP m special
       )
    => TextType enumType
    -> m [TextTree style enum special]
textForestP t = miForest elementPF (childPF t)

elementPF
    :: forall m style enum special
     . (MonadParser m, StyleP style, SpecialP m special)
    => m [TextTree style enum special]
    -> m (MiElementConfig, [TextTree style enum special])
elementPF p = fmap (maybeToList . fmap Special) <$> specialP <|> regularP
  where
    regularP :: m (MiElementConfig, [TextTree style enum special])
    regularP =
        fmap ((regularCfg,) . singleton) $
            Word <$> wordP (Proxy :: Proxy special)
                <|> Reference <$ char '{' <* char ':' <*> labelP <* char '}'
                <|> Styled <$ char '<' <*> styleP <*> p <* char '>'
      where
        regularCfg =
            MiElementConfig
                { miecPermitEnd = True
                , miecPermitChild = True
                , miecRetainTrailingWhitespace = True
                }

childPF
    :: forall m style enumType enum special
     . (ParserWrapper m, EnumP enumType enum, SpecialP m special)
    => TextType enumType
    -> m (TextTree style enum special)
childPF (TextType enumTypes footnoteTypes) =
    wrapParser (Enum <$> choice (fmap enumP enumTypes))
        <* postEnumP (Proxy :: Proxy special)
        <|> wrapParser (Footnote <$> choice (fmap footnoteTextP footnoteTypes))

footnoteTextP :: FootnoteType -> Parser [FootnoteTextTree]
footnoteTextP (FootnoteType kw tt) = hangingTextP kw tt

hangingTextP
    :: ( ParserWrapper m
       , StyleP style
       , EnumP enumType enum
       , SpecialP m special
       )
    => Keyword
    -> TextType enumType
    -> m [TextTree style enum special]
hangingTextP kw t = hangingBlock_ (keywordP kw) elementPF (childPF t)

hangingTextP'
    :: ( ParserWrapper m
       , StyleP style
       , EnumP enumType enum
       , SpecialP m special
       )
    => Keyword
    -> TextType enumType
    -> m (Maybe Label, [TextTree style enum special])
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

class EnumP enumType enum where
    enumP :: enumType -> Parser enum

instance EnumP Void Void where
    enumP = const empty

instance EnumP EnumType Enumeration where
    enumP (EnumType kw tt) = Enumeration <$> someIndented enumItemP
      where
        enumItemP = uncurry Node . fmap EnumItem <$> hangingTextP' kw tt

class SpecialP m special | special -> m where
    specialP :: m (MiElementConfig, Maybe special)
    wordP :: Proxy special -> m Text
    postEnumP :: Proxy special -> m ()

instance SpecialP Parser Void where
    specialP = empty

    wordP _ = gWordP isWordChar isWordSpecialChar

    postEnumP _ = pure ()

instance SpecialP ParagraphParser SentenceStart where
    specialP =
        fmap (specialCfg,) $
            Nothing <$ continueP
                <|> Just <$> sentenceStartP
      where
        -- Sentence start tokens (SSTs) must be followed by a regular element;
        -- that is, they must not occur at the end of their parent element
        -- (particularly, a paragraph), or directly preceding a text child.
        --  - This is enforced via `specialCfg`.
        --  - Note that unlabeled (i.e., empty) SSTs
        --    - are impossible before text children and at a paragraph's end
        --      anyways (unless on their own (empty) line, see below), and
        --    - could also be appropriately restricted by ensuring they are
        --      not followed by `>`, `\n`, or EOF.
        --
        -- Further, unlabeled SSTs must not occur as only element of an input
        -- line (for that input line would be empty).
        --  - We prohibit this by checking for a succeeding newline character.
        --  - The EOF case is already covered by
        --    `specialCfg { miecPermitEnd = False }`.
        --
        -- Otherwise, labeled SSTs may occur whenever the state permits,
        -- while unlabeled SSTs are not permitted before an opening styling
        -- tag (`<X` for some `X`).
        --  - Parsing an unlabeled SST is delayed until after the opening
        --    styling tag, while a labeled SST may occur either before or
        --    after an opening styling tag.

        specialCfg =
            MiElementConfig
                { miecPermitEnd = False
                , miecPermitChild = False
                , miecRetainTrailingWhitespace = False
                }

        sentenceStartP = do
            isSentenceStartExpected <- get
            if isSentenceStartExpected
                then (labeledSSP <|> unlabeledSSP) <* put False
                else empty
          where
            -- TODO: Avoid `try`.
            labeledSSP :: ParagraphParser SentenceStart
            labeledSSP = SentenceStart . Just <$> try labelingP

            unlabeledSSP :: ParagraphParser SentenceStart
            unlabeledSSP =
                SentenceStart Nothing <$ notFollowedBy (char '\n' <|> char '<')

        -- The `{>}` token means to continue the current sentence.
        --  - It is meant to be used after an enumeration, but can be used
        --    anywhere where a sentence start is permitted.
        --  - The same rules as for labeled SSTs apply w.r.t. placement.
        continueP = do
            isSentenceStartExpected <- get
            _ <- string "{>}"
            if isSentenceStartExpected
                then put False
                else fail "Unexpected sentence continuation token."

    wordP _ = sentenceWordP <|> sentenceEndP
      where
        sentenceWordP :: ParagraphParser Text
        sentenceWordP = gWordP isWordChar isSentenceSpecialChar

        sentenceEndP :: ParagraphParser Text
        sentenceEndP = Text.singleton <$> satisfy isSentenceEndChar <* put True

    -- An enumeration child ends a sentence.
    postEnumP _ = put True

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
