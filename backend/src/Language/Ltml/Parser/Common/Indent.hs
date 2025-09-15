{-# LANGUAGE FlexibleContexts #-}

-- | Parsers and parser combinators for handling indented text.
--
--   They generally expect to be run at the start of an input line, after any
--   indentation (ASCII spaces; usually after 'nli').
--   (Compare how typical lexeme parser combinators are expected to be run
--   after any whitespace.)
module Language.Ltml.Parser.Common.Indent
    ( nli
    , someIndented
    , checkIndentGT
    )
where

import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Data.Text (Text)
import qualified Data.Text as Text (singleton)
import Language.Ltml.Parser (MonadParser)
import Language.Ltml.Parser.Common.Lexeme (lineCommentP)
import Text.Megaparsec
    ( Pos
    , sepBy1
    , takeWhileP
    )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L
    ( incorrectIndent
    , indentLevel
    )

-- | Parse a newline character, any number of comment lines, and any
--   subsequent indentation (ASCII spaces).
--
--   Comment lines are lines that only contain indentation followed by a
--   line comment.
--
--   Always returns a single newline character.
nli :: (MonadParser m) => m Text
nli = Text.singleton <$> char '\n' <* indentationP

-- | Parse indentation, dropping any full line comments.
indentationP :: (MonadParser m) => m ()
indentationP = void $ sepBy1 indP (lineCommentP >> char '\n')
  where
    indP = takeWhileP (Just "indentation") (== ' ')

-- | Parse some (>= 1) items, all indented further than the provided reference
--   indentation level, but not necessarily the same amount.
--   For the first item, the indentation is not checked, and should thus be
--   checked by the caller.
--
--   The argument parser must not accept the empty input and must only succeed
--   after a final newline plus any trailing indentation.
someIndented :: (MonadParser m) => Maybe Pos -> m a -> m [a]
someIndented lvl p = p `sepBy1` checkIndentGT lvl

-- | Check whether the current actual indentation is greater than the supplied
--   reference indentation level.
checkIndentGT :: (MonadParser m) => Maybe Pos -> m ()
checkIndentGT Nothing = return ()
checkIndentGT (Just lvl) = do
    pos <- L.indentLevel
    guard (pos > lvl) <|> L.incorrectIndent GT lvl pos
