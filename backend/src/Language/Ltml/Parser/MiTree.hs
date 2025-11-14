{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing Mixed Indentation Trees---trees that may both be represented by
--   indentation, or by bracketing tokens, where the latter nodes may span
--   multiple lines, while empty lines are disallowed.
module Language.Ltml.Parser.MiTree
    ( MiElementConfig (..)
    , InlineParser (..)
    , Restricted
    , unrestricted
    , unbracketed
    , miForest
    , hangingBlock
    , hangingBlock'
    , hangingBlock_
    , pipeSeparated
    )
where

import Control.Applicative (optional, (<|>))
import Control.Applicative.Combinators (choice)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Language.Ltml.Parser (MonadParser)
import Language.Ltml.Parser.Common.Indent
    ( checkIndentGT
    , nli
    )
import Language.Ltml.Parser.Common.Lexeme (nLexeme, sp)
import Text.Megaparsec (Pos, empty, lookAhead, many, try, (<?>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (indentLevel)
import qualified Text.Megaparsec.Char.Lexer as L (indentLevel)

-- | Configuration on how to handle an element (node in a mi-tree).
data MiElementConfig = MiElementConfig
    { miecRetainPrecedingWhitespace :: Bool
    -- ^ whether to retain (or else drop) whitespace between the preceding and
    --   this element (if any).
    , miecRetainTrailingWhitespace :: Bool
    -- ^ Whether to retain (or else drop) whitespace between this and
    --   the subsequent element (if any).
    --   This does not apply if the subsequent element is a child (in which
    --   case whitespace is always dropped).
    }

-- | An in-line element parser (constructor).
--   Involved parsers must not consume whitespace (ASCII spaces, newlines)
--   and must not accept the empty input.
data InlineParser m a
    = LeafParser (m (MiElementConfig, [a]))
    | -- | Bracketing parser, composed of two parsers, one for the opening
      --   bracket, one for the closing bracket.
      --   The closing bracket parser is only used if possible
      --   (via 'Control.Applicative.optional').
      --   The body parser is determined by context.
      --   Any whitespace both within and adjacent to the brackets is dropped.
      BracketingParser (m ([a] -> (MiElementConfig, [a]))) (m ())

data Restricted a = Restricted Restriction a

data Restriction
    = Unrestricted
    | Unbracketed
    deriving (Eq)

unrestricted :: a -> Restricted a
unrestricted = Restricted Unrestricted

unbracketed :: a -> Restricted a
unbracketed = Restricted Unbracketed

filterRestricted :: (Restriction -> Bool) -> [Restricted a] -> [a]
filterRestricted p = mapMaybe aux
  where
    aux (Restricted r x) = if p r then Just x else Nothing

-- | Parse a list of mixed indentation trees (a forest), terminated by a
--   newline (plus indentation).
--
--   At least one element is parsed.
--
--   This is expected to be run at the start of a non-empty line, after any
--   indentation.
--
--   The initial (minimum) indentation is set to none.
miForest
    :: forall m a
     . (MonadParser m, FromWhitespace [a])
    => [Restricted (InlineParser m a)]
    -- ^ In-line element parsers.
    --   They are tried in the given order.
    -> (Maybe Pos -> m a)
    -- ^ Block element parser.
    --
    --   The block parser is only attempted at the start of a line, and takes
    --   precedence over in-line parsers there.
    --
    --   Unlike the in-line parsers, the block parser must take care of
    --   indentation itself--except at the very beginning, where it may
    --   expect that any indentation has been consumed and the indentation is
    --   correct.
    --   The supplied indentation is the parent's, and indentation is
    --   acceptable iff strictly larger than that.
    --
    --   Further, the block parser must only succeed after a final newline
    --   (plus indentation).
    --
    --   Typically, a block parser is constructed via 'hangingBlock'
    --   (optionally combined with
    --   'Language.Ltml.Parser.Common.Indent.someIndented' and/or
    --   'Control.Applicative.<|>'), which satisfies these requirements.
    -> m [a]
miForest inlinePs blockP = miForestFrom False False inlinePs blockP Nothing

-- | Information on whitespace separating two elements.
data Sep
    = Sep
        Bool
        -- ^ whether including linebreak
        Text

-- | Generalization of 'miForest'.
miForestFrom
    :: forall m a
     . (MonadParser m, FromWhitespace [a])
    => Bool
    -- ^ Whether there has already been parsed a "head" in the current line.
    --   A head is any non-empty token (whitespace counting as empty).
    --   If @True@, the parser behaves as if an initial in-line element was
    --   already parsed.  In particular, returning successfully without
    --   parsing any element is possible.
    --   If @False@, the parser is expected to be run at the start of an
    --   (indented) non-empty line.
    -> Bool
    -- ^ Whether the miForest should be ended when encountering a '|',
    --   which is a semi-special word character.
    -> [Restricted (InlineParser m a)]
    -> (Maybe Pos -> m a)
    -> Maybe Pos
    -- ^ Parent indentation level.  Only input indented strictly further is
    --   accepted.
    -> m [a]
miForestFrom rootIsHeaded untilPipe rInlinePs blockP lvl = do
    (x, sep) <- go rootIsHeaded (filterRestricted (const True) rInlinePs)
    case sep of
        Sep True _ -> return x
        Sep False _ -> empty <?> "newline"
  where
    mkP :: InlineParser m a -> m ((MiElementConfig, [a]), Sep)
    mkP (LeafParser p) = (,) <$> p <*> sepP
    mkP (BracketingParser openP closeP) = do
        f <- openP
        (body, s) <- go True $ filterRestricted (/= Unbracketed) rInlinePs
        ms' <- optional (closeP *> sepP)
        return (f body, fromMaybe s ms')

    -- CONSIDER: Permit EOF.
    sepP :: m Sep
    sepP = do
        s <- sp
        ms' <- optional nli
        case ms' of
            Just s' -> return $ Sep True (s <> s')
            Nothing -> return $ Sep False s

    -- To be called at the start of an (indented) line iff not isHeaded.
    -- Iff `isHeaded`, treated as if a first in-line element was already
    -- parsed.
    --  - In particular, permits empty element list.
    go :: Bool -> [InlineParser m a] -> m ([a], Sep)
    go isHeaded inlinePs =
        if isHeaded
            then sepP >>= goTail False
            else goBlock <|> goInline mempty
      where
        goInline :: Text -> m ([a], Sep)
        goInline precWS = do
            -- check if '|' is next and should terminate the forest
            mStop <- optional $ lookAhead (try $ char '|')
            case (mStop, untilPipe) of
                (Just _, True) -> goEnd (Sep True precWS)
                _ -> do
                    ((cfg, e), s) <- choice $ map mkP inlinePs

                    let precWS' :: [a]
                        precWS' =
                            if miecRetainPrecedingWhitespace cfg
                                then fromWhitespace precWS
                                else []

                    (es, s') <- goTail (miecRetainTrailingWhitespace cfg) s

                    return (precWS' ++ e ++ es, s')

        goTail :: Bool -> Sep -> m ([a], Sep)
        goTail retainWS (Sep lineEnded precWS) =
            if lineEnded
                then checkIndentGT lvl *> (goBlock <|> goInline') <|> goEnd'
                else goInline' <|> goEnd'
          where
            goInline' = goInline precWS'
            goEnd' = goEnd (Sep lineEnded precWS')
            precWS' = if retainWS then precWS else mempty

        goBlock :: m ([a], Sep)
        goBlock = do
            x <- blockP lvl
            (xs, s) <- goTail False (Sep True mempty)
            return (x : xs, s)

        goEnd :: Sep -> m ([a], Sep)
        goEnd s = pure ([], s)

-- | Parse a mi-forest headed by a keyword, with all lines but the first
--   indented further than the first.
--
--   Text may begin on the line of the keyword or on the next (indented) line.
--
--   The documentation on 'miForest' generally applies.
hangingBlock
    :: (MonadParser m, FromWhitespace [a])
    => m ([a] -> b)
    -- ^ Keyword parser.  Result is applied to the parsed mi-forest.
    --   This is expected not to consume any whitespace.
    -> [Restricted (InlineParser m a)]
    -> (Maybe Pos -> m a)
    -> m b
hangingBlock keywordP inlinePs blockP = do
    lvl' <- L.indentLevel
    keywordP <*> miForestFrom True False inlinePs blockP (Just lvl')

-- | Version of 'hangingBlock' where the keyword parser may yield any value,
--   which is paired with the parsed mi-forest.
hangingBlock'
    :: (MonadParser m, FromWhitespace [a])
    => m b
    -> [Restricted (InlineParser m a)]
    -> (Maybe Pos -> m a)
    -> m (b, [a])
hangingBlock' = hangingBlock . fmap (,)

-- | Version of 'hangingBlock' where the keyword parser does not return a
--   value.
hangingBlock_
    :: (MonadParser m, FromWhitespace [a])
    => m ()
    -> [Restricted (InlineParser m a)]
    -> (Maybe Pos -> m a)
    -> m [a]
hangingBlock_ = hangingBlock . fmap (const id)

-- | Parse a list of '|' seperated TextForests.
--   At least one element will be parsed.
--   The list can also be defined across multiple lines
--   and contain empty lines.
--   The initial (minimum) indentation is set to none.
pipeSeparated
    :: forall m a
     . (MonadParser m, FromWhitespace [a])
    => [Restricted (InlineParser m a)]
    -> (Maybe Pos -> m a)
    -- ^ Block parser. Documentation of 'miForest' applies.
    -> m [[a]]
pipeSeparated inlinePs blockP = do
    lvl <- indentLevel
    pipeSeparatedFrom inlinePs blockP (Just lvl)

pipeSeparatedFrom
    :: forall m a
     . (MonadParser m, FromWhitespace [a])
    => [Restricted (InlineParser m a)]
    -> (Maybe Pos -> m a)
    -- ^ Block parser. Documentation of 'miForest' applies.
    -> Maybe Pos
    -- ^ Parent indentation level. Documentation of 'miForestFrom' applies.
    -> m [[a]]
pipeSeparatedFrom inlinePs blockP lvl = do
    first <- miForestFrom False True inlinePs blockP lvl
    rest <- many (nLexeme (char '|') *> miForestFrom True True inlinePs blockP lvl)
    return (first : rest)
