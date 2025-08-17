{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Common.Lexeme
    ( nLexeme
    , nLexeme1
    )
where

import Control.Monad (void)
import Language.Ltml.Parser (MonadParser, sp)
import Text.Megaparsec (takeWhileP)
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L (lexeme)

-- | Lexeme parser combinator that permits newlines and ASCII spaces.
nLexeme :: (MonadParser m) => m a -> m a
nLexeme = L.lexeme nSc

-- | Like 'nLexeme', but require at least one newline.
--   Useful for parsers that do not themselves require a final newline.
nLexeme1 :: (MonadParser m) => m a -> m a
nLexeme1 p = nLexeme (p <* sp <* char '\n')

nSc :: (MonadParser m) => m ()
nSc = void $ takeWhileP (Just "whitespace") (\c -> c == '\n' || c == ' ')
