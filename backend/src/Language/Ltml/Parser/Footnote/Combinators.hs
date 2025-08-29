{-# LANGUAGE ScopedTypeVariables #-}

module Language.Ltml.Parser.Footnote.Combinators
    ( manyWithFootnotesTillSucc
    )
where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Data.Maybe (catMaybes)
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Combinators (manyTillSucc)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Footnote (FootnoteParser, footnoteP)

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
