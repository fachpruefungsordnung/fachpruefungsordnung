{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.DocumentContainer
    ( documentContainerHeaderP
    )
where

import Data.Text (Text, unwords)
import Language.Ltml.AST.DocumentContainer (DocumentContainerHeader (..))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (lexeme, nLexeme1, nSc, symbol)
import Language.Ltml.Parser.Text (rawWordP)
import Text.Megaparsec (many)
import Prelude hiding (unwords)

-- TODO: Permit different ordering.
-- TODO: Normalize comment syntax.
--  - Here, we do not recognice a comment in `word// comment` (due to the
--    missing space), but usually we do.
-- TODO: Document syntax.
documentContainerHeaderP :: Parser DocumentContainerHeader
documentContainerHeaderP = do
    nSc
    pdfTitle <- entryP "pdf-title"
    hfSuperTitle <- entryP "header-footer-supertitle"
    hfTitle <- entryP "header-footer-title"
    hfDate <- entryP "header-footer-date"
    return $
        DocumentContainerHeader
            { dchPdfTitle = pdfTitle
            , dchHeaderFooterSuperTitle = hfSuperTitle
            , dchHeaderFooterTitle = hfTitle
            , dchHeaderFooterDate = hfDate
            }
  where
    entryP :: Text -> Parser Text
    entryP prefix = nLexeme1 $ do
        symbol (prefix <> ":")
        unwords <$> many (lexeme rawWordP)
