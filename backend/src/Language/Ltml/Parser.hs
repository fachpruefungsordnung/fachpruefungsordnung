{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ltml.Parser
    ( Parser
    , MonadParser
    , ParserWrapper (wrapParser)
    , ParserWrapper (wrapParser)
    , nli
    , nextIndentLevel
    , checkIndent
    , eoi
    )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as Text (cons)
import Data.Void (Void)
import Text.Megaparsec
    ( MonadParsec
    , Parsec
    , Pos
    , eof
    , mkPos
    , takeWhileP
    , (<?>)
    )
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L (indentLevel)

type Parser = Parsec Void Text

type MonadParser m = (MonadParsec Void Text m, MonadFail m)

class ParserWrapper m where
    wrapParser :: Parser a -> m a

instance ParserWrapper Parser where
    wrapParser = id

nextIndentLevel :: Pos -> Pos
nextIndentLevel = (<> mkPos 2)

-- | Parse a newline character and any subsequent indentation (ASCII spaces).
nli :: (MonadParser m) => m Text
nli =
    Text.cons
        <$> char '\n'
        <*> takeWhileP (Just "indentation") (== ' ')

-- | Check whether the current actual indentation matches the current required
--   indentation level.
--   This parser is expected to be run at the start of an input line, after
--   any indentation (ASCII spaces; usually after 'nli').
checkIndent :: (MonadParser m) => Pos -> m ()
checkIndent lvl = do
    pos <- L.indentLevel
    guard (pos == lvl) <|> fail "Incorrect indentation."

-- | Check for End Of Indentation scope (whether actual indentation is less
--   then current indentation level, or eof is reached).
eoi :: (MonadParser m) => Pos -> m ()
eoi lvl = (decrIndent <|> eof) <?> "end of indentation scope"
  where
    decrIndent = L.indentLevel >>= guard . (< lvl)
