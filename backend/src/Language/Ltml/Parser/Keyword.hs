{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Keyword
    ( keywordP
    , mlKeywordP
    )
where

import Control.Applicative (optional)
import Control.Monad (void)
import Language.Lsd.AST.Common (Keyword (Keyword))
import Language.Ltml.AST.Label (Label)
import Language.Ltml.Parser (MonadParser)
import Language.Ltml.Parser.Label (labelingP)
import Text.Megaparsec.Char (string)

keywordP :: (MonadParser m) => Keyword -> m ()
keywordP (Keyword kw) = void $ string kw

mlKeywordP :: (MonadParser m) => Keyword -> m (Maybe Label)
mlKeywordP (Keyword kw) = string kw *> optional labelingP
