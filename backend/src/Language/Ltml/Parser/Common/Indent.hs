{-# LANGUAGE FlexibleContexts #-}

-- | Parsers and parser combinators for handling indented text.
--
--   They generally expect to be run at the start of an input line, after any
--   indentation (ASCII spaces; usually after 'nli').
--   (Compare how typical lexeme parser combinators are expected to be run
--   after any whitespace.)
module Language.Ltml.Parser.Common.Indent
    ( nli
    , nextIndentLevel
    , nonIndented
    , someIndented
    , checkIndent
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
    , mkPos
    , sepBy1
    , takeWhileP
    )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L
    ( incorrectIndent
    , indentLevel
    )
import Text.Megaparsec.Pos (pos1)

nextIndentLevel :: Pos -> Pos
nextIndentLevel = (<> mkPos 2)

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

-- | Given a parser, adapt it to parse non-indented data (only).
nonIndented :: (MonadParser m) => m a -> m a
nonIndented p = checkIndent pos1 *> p

-- | Parse some (>= 1) indented items, all with the same indentation level.
--   The argument parser is expected to consume any trailing indentation.
someIndented :: (MonadParser m) => m a -> m [a]
someIndented p = L.indentLevel >>= (\lvl -> p `sepBy1` checkIndent lvl)

-- | Check whether the current actual indentation matches the current required
--   indentation level.
checkIndent :: (MonadParser m) => Pos -> m ()
checkIndent lvl = do
    pos <- L.indentLevel
    guard (pos == lvl) <|> L.incorrectIndent EQ lvl pos
