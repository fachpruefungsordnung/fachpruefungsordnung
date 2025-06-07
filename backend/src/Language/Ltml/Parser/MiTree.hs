{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing Mixed Indentation Trees---trees that may both be represented by
--   indentation, or by bracketing tokens, where the latter nodes may span
--   multiple lines of matching indentation.
module Language.Ltml.Parser.MiTree
    ( miForest
    , hangingBlock
    , hangingBlock'
    , hangingBlock_
    , sp
    , sp1
    )
where

import Control.Applicative (empty, (<|>))
import Control.Applicative.Utils ((<:>))
import Control.Monad (void)
import Data.Text (Text)
import Data.Text.FromWhitespace (FromWhitespace, fromWhitespace)
import Language.Ltml.Parser
    ( MonadParser
    , checkIndent
    , eoi
    , nextIndentLevel
    , nli
    )
import Text.Megaparsec (Pos, takeWhile1P, takeWhileP)
import qualified Text.Megaparsec.Char.Lexer as L (indentLevel)

sp :: (MonadParser m) => m Text
sp = takeWhileP (Just "spaces") (== ' ')

sp1 :: (MonadParser m) => m Text
sp1 = takeWhile1P (Just "spaces") (== ' ')

sp' :: (MonadParser m, FromWhitespace a) => m a
sp' = fromWhitespace <$> sp

nli' :: (MonadParser m, FromWhitespace a) => m a
nli' = fromWhitespace <$> nli

-- | Parse a list of mixed indentation trees (a forest).
--
--   In-line nodes are parsed by @'elementPF' p@, where @p@ is supplied as the
--   parser for the children nodes.
--
--   Indented nodes are parsed by 'childP'.
--
--   'elementPF' is expected to be a simple wrapper around its argument @p@,
--   and not to consume (ASCII) spaces or newlines, unlike @p@.
--   @p@ only succeeds on an in-line ending; that is, it fails on a final
--   newline or EOF.
--   Typically, @p@ is enclosed in some kind of bracketing parsers.
--   For leaf nodes, 'elementPF' may simply ignore @p@.
--   @'elementPF' p@ is further expected to not accept the empty input.
--
--   Unlike 'elementPF', 'childP' is expected to take care of indentation
--   itself--except at the very beginning, where it may expect that any
--   indentation has been parsed and the indentation is correct.
--   Further, 'childP' must only succeed after a final newline (plus
--   indentation) or EOF (see also 'eoi').
--   Typically, 'childP' uses 'hangingBlock', which satisfies these
--   requirements.
miForest
    :: forall m a
     . (MonadParser m, FromWhitespace a)
    => (m [a] -> m a)
    -> m a
    -> m [a]
miForest elementPF childP = L.indentLevel >>= miForestFrom elementPF childP

miForestFrom
    :: forall m a
     . (MonadParser m, FromWhitespace a)
    => (m [a] -> m a)
    -> m a
    -> Pos
    -> m [a]
miForestFrom elementPF childP lvl = go True
  where
    go :: Bool -> m [a]
    go permitEndAfterNewline = goE
      where
        -- `goX` vs. `goX'`:
        --  - The `goX` parsers must generally be used in-line; that is, not
        --    at the start of a line.
        --    - Exception: `goE`, initially.
        --  - The `goX'` parsers must only be used at the start of a line
        --    (after indentation; i.e., after `nli'`).

        -- Parse forest, headed by element.
        goE :: m [a]
        goE =
            elementPF (go False)
                <:> ( (nli' >>= \s -> (s :) <$> goE' <|> goC' <|> goEnd')
                        <|> sp' <:> goE
                        <|> goEnd
                    )

        goE' :: m [a]
        goE' = checkIndent lvl *> goE

        -- Parse forest, headed by child; at start of an input line.
        goC' :: m [a]
        goC' = checkIndent lvl' *> childP <:> (goE' <|> goC' <|> goEnd')
          where
            lvl' = nextIndentLevel lvl

        goEnd :: m [a]
        goEnd = pure []

        goEnd' :: m [a]
        goEnd' =
            if permitEndAfterNewline
                then goEnd
                else empty

-- | Parse a mi-forest headed by a keyword, with all lines but the first
--   indented one additional level.
--
--   Text may begin on the line of the keyword, with at least one separating
--   (ASCII) space, or on the next (indented) line.
--
--   The result of 'keywordP' is a function that is applied to the parsed
--   mi-forest.
--   'keywordP' is expected not to consume any whitespace.
--
--   For the other arguments, see 'miForest'.
hangingBlock
    :: (MonadParser m, FromWhitespace a)
    => m ([a] -> b)
    -> (m [a] -> m a)
    -> m a
    -> m b
hangingBlock keywordP elementPF childP = do
    lvl' <- nextIndentLevel <$> L.indentLevel
    f <- keywordP
    void sp1 <|> void nli <* checkIndent lvl'
    f <$> miForestFrom elementPF childP lvl' <* eoi lvl'

-- | Version of 'hangingBlock' where the keyword parser may yield any value,
--   which is paired with the parsed mi-forest.
hangingBlock'
    :: (MonadParser m, FromWhitespace a)
    => m b
    -> (m [a] -> m a)
    -> m a
    -> m (b, [a])
hangingBlock' = hangingBlock . fmap (,)

-- | Version of 'hangingBlock' where the keyword parser does not return a
--   value.
hangingBlock_
    :: (MonadParser m, FromWhitespace a)
    => m ()
    -> (m [a] -> m a)
    -> m a
    -> m [a]
hangingBlock_ = hangingBlock . fmap (const id)
