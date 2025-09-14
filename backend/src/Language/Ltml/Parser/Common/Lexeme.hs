{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Common.Lexeme
    ( sp
    , sp1
    , lexeme
    , symbol
    , nSc
    , nLexeme
    , nLexeme1
    , lineCommentP
    , isLineCommentPrefixFirstChar
    )
where

import Control.Applicative (empty, optional)
import Control.Monad (void)
import Data.Text (Text)
import Language.Ltml.Parser (MonadParser)
import Text.Megaparsec (takeWhile1P, takeWhileP, (<?>))
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L
    ( lexeme
    , skipLineComment
    , space
    , symbol
    )

-- TODO: Use.

-- | Lexeme parser combinator that permits ASCII spaces and line comments, but
--   no newlines.
lexeme :: (MonadParser m) => m a -> m a
lexeme = L.lexeme (void sp)

symbol :: (MonadParser m) => Text -> m ()
symbol = void . L.symbol (void sp)

-- | Space parser (accepts any number of ASCII spaces and, optionally, a final
--   line comment).
sp :: (MonadParser m) => m Text
sp = takeWhileP (Just "space") (== ' ') <* optional lineCommentP

-- | Space parser (accepts one or more ASCII spaces and, optionally, a final
--   line comment).
sp1 :: (MonadParser m) => m Text
sp1 = takeWhile1P (Just "space") (== ' ') <* optional lineCommentP

-- | Lexeme parser combinator that permits newlines, ASCII spaces, and line
--   comments.
nLexeme :: (MonadParser m) => m a -> m a
nLexeme = L.lexeme nSc

-- | Like 'nLexeme', but require at least one newline.
--   Useful for parsers that do not themselves require a final newline.
nLexeme1 :: (MonadParser m) => m a -> m a
nLexeme1 p = nLexeme (p <* sp <* char '\n')

nSc :: (MonadParser m) => m ()
nSc =
    L.space
        (void $ takeWhile1P (Just "whitespace") (\c -> c == '\n' || c == ' '))
        lineCommentP
        empty

lineCommentP :: (MonadParser m) => m ()
lineCommentP = L.skipLineComment "//" <?> "line comment"

isLineCommentPrefixFirstChar :: Char -> Bool
isLineCommentPrefixFirstChar '/' = True
isLineCommentPrefixFirstChar _ = False
