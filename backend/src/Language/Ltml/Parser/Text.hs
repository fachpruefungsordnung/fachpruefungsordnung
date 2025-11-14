{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.Parser.Text
    ( ParagraphParser
    , textForestP
    , pipeSeperatedTextForestsP
    , hangingTextP
    , HangingTextP
    , hangingTextP'
    , rawWordP
    )
where

import Control.Applicative (empty, (<|>))
import Control.Applicative.Combinators (choice)
import Control.Monad (guard, void)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans.Class (lift)
import qualified Data.Char as Char (isControl)
import Data.List (singleton)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as Text (singleton)
import Data.Typography (FontStyle (..))
import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction))
import Language.Lsd.AST.Type (unwrapNT)
import Language.Lsd.AST.Type.Enum (EnumType (EnumType))
import Language.Lsd.AST.Type.Text (TextType (TextType))
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Text
    ( EnumItem (EnumItem)
    , Enumeration (Enumeration)
    , FootnoteReference (FootnoteReference)
    , HardLineBreak (HardLineBreak)
    , SentenceStart (SentenceStart)
    , TextTree (..)
    )
import Language.Ltml.Parser
    ( MonadParser
    , Parser
    , ParserWrapper (wrapParser)
    )
import Language.Ltml.Parser.Common.Indent (someIndented)
import Language.Ltml.Parser.Common.Lexeme (isLineCommentPrefixFirstChar)
import Language.Ltml.Parser.Keyword (keywordP, lKeywordP, mlKeywordP)
import Language.Ltml.Parser.Label (bracedLabelingP, labelP)
import Language.Ltml.Parser.MiTree
    ( InlineParser (BracketingParser, LeafParser)
    , MiElementConfig (..)
    , Restricted
    , hangingBlock'
    , hangingBlock_
    , miForest
    , pipeSeparated
    , unbracketed
    , unrestricted
    )
import Text.Megaparsec
    ( Pos
    , lookAhead
    , optional
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
       , LineBreakP lbrk
       , FootnoteRefP fnref
       , StyleP style
       , EnumP enumType enum
       , SpecialP m special
       )
    => TextType enumType
    -> m [TextTree lbrk fnref style enum special]
textForestP t = miForest inlinePs (blockPF t)

pipeSeperatedTextForestsP
    :: ( ParserWrapper m
       , LineBreakP lbrk
       , FootnoteRefP fnref
       , StyleP style
       , EnumP enumType enum
       , SpecialP m special
       )
    => TextType enumType
    -> m [[TextTree lbrk fnref style enum special]]
pipeSeperatedTextForestsP tt = pipeSeparated inlinePs (blockPF tt)

-- Note on sentence start tokens (SSTs):
--  * Labeled SSTs are permitted anywhere, while unlabeled SSTs are only
--    permitted in certain places.
--     - In particular, in case of styling, the default (empty) SST is parsed
--       after the opening styling tag, but can be forced before by a labeled
--       SST.
inlinePs
    :: forall m lbrk fnref style enum special
     . ( MonadParser m
       , LineBreakP lbrk
       , FootnoteRefP fnref
       , StyleP style
       , SpecialP m special
       )
    => [Restricted (InlineParser m (TextTree lbrk fnref style enum special))]
inlinePs =
    [ unrestricted $ LeafParser $ mkXP_ (NonBreakingSpace <$ char '~')
    , unrestricted $ LeafParser $ try (char '{' *> bracedP <* char '}')
    , unrestricted styledElementP
    , unrestricted $ LeafParser $ mkP (Word <$> wordP (Proxy :: Proxy special))
    , unbracketed $ LeafParser $ mkP (Word . Text.singleton <$> char '>')
    ]
  where
    -- This should not be expensive, it is indirectly wrapped in `try`.
    bracedP =
        mkP (Reference <$ char ':' <*> labelP)
            <|> mkP (FootnoteRef <$> bracedFootnoteRefP)
            <|> fmap (singleton . LineBreak) <$> bracedLineBreakP
            <|> fmap (maybeToList . fmap Special) <$> bracedSpecialP

    styledElementP =
        BracketingParser
            (try $ mkP_' $ Styled <$ char '<' <*> styleP)
            (void $ char '>')
      where
        mkP_' :: m (a -> b) -> m (a -> (MiElementConfig, [b]))
        mkP_' = fmap (((regularCfg,) . singleton) .)

    -- Note: Using mkP and mkXP_ repeatedly instead of grouping appropriately
    --   is maybe less efficient, but IMHO better readable.

    -- Make a simple parser, permitting preceding empty SST.
    mkP p =
        (regularCfg,) <$> do
            mSst <- optional $ Special <$> emptySentenceStartP
            x <- p
            return $ maybe id (:) mSst [x]

    -- Make a simple parser, not permitting preceding empty SST, and dropping
    -- surrounding whitespace.
    mkXP_ = fmap ((specialCfg,) . singleton)

    regularCfg =
        MiElementConfig
            { miecRetainPrecedingWhitespace = True
            , miecRetainTrailingWhitespace = True
            }

    specialCfg =
        MiElementConfig
            { miecRetainPrecedingWhitespace = False
            , miecRetainTrailingWhitespace = False
            }

blockPF
    :: forall m lbrk fnref style enumType enum special
     . (ParserWrapper m, EnumP enumType enum, SpecialP m special)
    => TextType enumType
    -> Maybe Pos
    -> m (TextTree lbrk fnref style enum special)
blockPF (TextType (Disjunction enumTypes)) lvl =
    wrapParser (Enum <$> choice (map (enumP lvl . unwrapNT) enumTypes))
        <* postEnumP (Proxy :: Proxy special)

-- TODO: Unused.
hangingTextP
    :: ( ParserWrapper m
       , LineBreakP lbrk
       , FootnoteRefP fnref
       , StyleP style
       , EnumP enumType enum
       , SpecialP m special
       )
    => Keyword
    -> TextType enumType
    -> m [TextTree lbrk fnref style enum special]
hangingTextP kw t = runIdentity <$> hangingTextP' kw t

class (Functor f) => HangingTextP f where
    hangingTextP'
        :: ( ParserWrapper m
           , LineBreakP lbrk
           , FootnoteRefP fnref
           , StyleP style
           , EnumP enumType enum
           , SpecialP m special
           )
        => Keyword
        -> TextType enumType
        -> m (f [TextTree lbrk fnref style enum special])

instance HangingTextP Identity where
    hangingTextP' kw t =
        Identity <$> hangingBlock_ (keywordP kw) inlinePs (blockPF t)

instance HangingTextP Node where
    hangingTextP' kw t = uncurry Node <$> hangingTextP' kw t

instance HangingTextP ((,) Label) where
    hangingTextP' kw t = hangingBlock' (lKeywordP kw) inlinePs (blockPF t)

instance HangingTextP ((,) (Maybe Label)) where
    hangingTextP' kw t = hangingBlock' (mlKeywordP kw) inlinePs (blockPF t)

class LineBreakP lbrk where
    bracedLineBreakP :: (MonadParser m) => m (MiElementConfig, lbrk)

instance LineBreakP Void where
    bracedLineBreakP = empty

instance LineBreakP HardLineBreak where
    bracedLineBreakP = (cfg, HardLineBreak) <$ string "nl"
      where
        cfg =
            MiElementConfig
                { miecRetainPrecedingWhitespace = False
                , miecRetainTrailingWhitespace = False
                }

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
    enumP :: Maybe Pos -> enumType -> Parser enum

instance EnumP Void Void where
    enumP _ _ = empty

instance EnumP EnumType Enumeration where
    enumP lvl (EnumType kw fmt tt) =
        Enumeration fmt <$> someIndented lvl enumItemP
      where
        enumItemP = uncurry Node . fmap EnumItem <$> hangingTextP' kw tt

class SpecialP m special | special -> m where
    emptySentenceStartP :: m special
    bracedSpecialP :: m (MiElementConfig, Maybe special)
    wordP :: Proxy special -> m Text
    postEnumP :: Proxy special -> m ()

instance SpecialP Parser Void where
    emptySentenceStartP = empty

    bracedSpecialP = empty

    wordP _ = gWordP isWordChar isWordSemiSpecialChar isWordSpecialChar

    postEnumP _ = pure ()

-- | Parse iff the paragraph state permits an SST and reset the state if
--   parsing successful.
--   Unrelated to 'try'.
sspTry :: ParagraphParser a -> ParagraphParser a
sspTry p = get >>= guard >> (p <* put False)

instance SpecialP ParagraphParser SentenceStart where
    emptySentenceStartP = sspTry $ pure $ SentenceStart Nothing

    bracedSpecialP =
        fmap (specialCfg,) $
            sspTry $
                Nothing <$ continueP
                    <|> Just <$> labeledSSP
      where
        specialCfg =
            MiElementConfig
                { miecRetainPrecedingWhitespace = True
                , miecRetainTrailingWhitespace = False
                }

        labeledSSP = SentenceStart . Just <$> bracedLabelingP

        continueP = void (char '>')

    wordP _ = sentenceWordP <|> sentenceEndP
      where
        sentenceWordP :: ParagraphParser Text
        sentenceWordP =
            gWordP isWordChar isWordSemiSpecialChar isSentenceSpecialChar

        sentenceEndP :: ParagraphParser Text
        sentenceEndP = Text.singleton <$> satisfy isSentenceEndChar <* put True

    -- An enumeration ends a sentence.
    postEnumP _ = put True

class FootnoteRefP fnref where
    bracedFootnoteRefP :: (MonadParser m) => m fnref

instance FootnoteRefP Void where
    bracedFootnoteRefP = empty

instance FootnoteRefP FootnoteReference where
    bracedFootnoteRefP = FootnoteReference <$ string "^:" <*> labelP

-- | Construct a word parser.
--
--   The first predicate determines valid characters, the second semi-special
--   characters, and the third special characters.
--
--   ASCII spaces and newlines must not count as valid.
--
--   Only valid characters are permitted in a word.
--
--   All valid characters may be escaped.
--   Special characters must be escaped.
--
--   Semi-special characters, if unescaped, form a full word.
gWordP
    :: (MonadParser m)
    => (Char -> Bool)
    -> (Char -> Bool)
    -> (Char -> Bool)
    -> m Text
gWordP isValid isSemiSpecial isSpecial =
    mconcat <$> some (regularWordP <|> escapedCharP) <|> semiSpecialCharP
  where
    regularWordP :: (MonadParser m) => m Text
    regularWordP = takeWhile1P (Just "regular character") isRegular
      where
        isRegular :: Char -> Bool
        isRegular c = isValid c && not (isSemiSpecial c || isSpecial c)

    semiSpecialCharP :: (MonadParser m) => m Text
    semiSpecialCharP = Text.singleton <$> satisfy isSemiSpecial

    escapedCharP :: (MonadParser m) => m Text
    escapedCharP =
        Text.singleton
            <$ char '\\'
            <*> ( satisfy isValid
                    -- The lookAhead is purely for better error messages.
                    <|> '\\' <$ lookAhead (satisfy isWordSepChar)
                )

-- TODO?: Handle `/`.
rawWordP :: (MonadParser m) => m Text
rawWordP = takeWhile1P (Just "word character") isWordChar

isWordSepChar :: Char -> Bool
isWordSepChar ' ' = True
isWordSepChar '\n' = True
isWordSepChar _ = False

-- NOTE: isControl '\n' == True
isWordChar :: Char -> Bool
isWordChar ' ' = False
isWordChar c = not $ Char.isControl c

isWordSemiSpecialChar :: Char -> Bool
isWordSemiSpecialChar '{' = True
isWordSemiSpecialChar '<' = True
isWordSemiSpecialChar '|' = True
isWordSemiSpecialChar c = isLineCommentPrefixFirstChar c

isWordSpecialChar :: Char -> Bool
isWordSpecialChar '\\' = True
isWordSpecialChar '>' = True
isWordSpecialChar '~' = True
isWordSpecialChar _ = False

isSentenceEndChar :: Char -> Bool
isSentenceEndChar '.' = True
isSentenceEndChar '!' = True
isSentenceEndChar '?' = True
isSentenceEndChar _ = False

isSentenceSpecialChar :: Char -> Bool
isSentenceSpecialChar c = isWordSpecialChar c || isSentenceEndChar c
