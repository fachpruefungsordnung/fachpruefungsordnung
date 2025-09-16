{-# LANGUAGE ScopedTypeVariables #-}

module Language.Ltml.Parser.Footnote.Combinators
    ( withSucceedingFootnotes
    , manyWithFootnotesTillSucc
    )
where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (catMaybes)
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Combinators (manyTillSucc)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Footnote (FootnoteParser, footnoteP)
import Text.Megaparsec (many)

-- | Parse with any succeeding footnotes, consuming any empty lines between
--   footnotes and finally.
--
--   The supplied argument parser must not succeed in-line, and must consume
--   any final whitespace.  I.e., it may only succeed after a newline plus any
--   subsequent whitespace.
withSucceedingFootnotes :: Parser a -> FootnoteParser a
withSucceedingFootnotes p = lift p <* many (nLexeme footnoteP)

-- | Like 'manyTillSucc', but parse any interleaved footnotes, and consume
--   any number of empty lines between nodes (including footnotes) and
--   finally.
manyWithFootnotesTillSucc
    :: forall a
     . Parser a
    -> Parser ()
    -> FootnoteParser [a]
manyWithFootnotesTillSucc p end =
    catMaybes <$> manyTillSucc (nLexeme elemP) (lift end)
  where
    elemP :: FootnoteParser (Maybe a)
    -- Note: `p` must be tried last.
    --  - It typically includes a paragraph parser, which generally treats
    --    keywords (as used for footnotes) as plain text.
    elemP =
        Nothing <$ footnoteP
            <|> Just <$> lift p
