{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Module (moduleBlockP, moduleP) where

import qualified Data.Char as Char
import Language.Lsd.AST.Type.Module (ModuleSchemaType (..), ModuleType (..))
import Language.Ltml.AST.Module
    ( Attribute (..)
    , Module (..)
    , ModuleBlock (..)
    , ModuleSchema (..)
    )
import Language.Ltml.Parser (MonadParser, Parser)
import Language.Ltml.Parser.Common.Indent (someIndented)
import Language.Ltml.Parser.Common.Lexeme (lexeme, nLexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Text.Megaparsec (many, sepBy1, takeWhile1P, (<?>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (indentLevel)

schemaP :: (MonadParser m) => ModuleSchemaType -> m ModuleSchema
schemaP (ModuleSchemaType kw) = do
    lexeme $ keywordP kw
    ModuleSchema
        <$> lexeme attributeP `sepBy1` lexeme (char ',') <?> "module schema"

-- | Permits ASCII Lower / Upper / Digit and '_', '-', '?', '/' characters.
--   This is used for schema attributes AND module values of those attributes.
attributeP :: (MonadParser m) => m Attribute
attributeP = Attribute <$> tokenP <?> "attribute"
  where
    isValid c =
        Char.isAsciiLower c
            || Char.isAsciiUpper c
            || Char.isDigit c
            || c `elem` "_-?/"
    tokenP = takeWhile1P (Just "attribute character") isValid

-------------------------------------------------------------------------------

moduleP :: ModuleType -> Parser Module
moduleP (ModuleType kw) = do
    lvl <- indentLevel
    nLexeme $ keywordP kw
    values <- someIndented (Just lvl) valueListP
    return $ Module $ concat values
  where
    -- The ',' consumes trailing newlines which allows stuff like:
    --
    -- keyword: ValueA,
    --   ValueB
    --
    -- But note that the ',' has to occur before the newline
    valueListP = lexeme attributeP `sepBy1` nLexeme (char ',')

moduleBlockP
    :: ModuleSchemaType -> ModuleType -> Parser ModuleBlock
moduleBlockP sType mType = do
    schema <- nLexeme (schemaP sType)
    modules <- many (nLexeme (moduleP mType)) <?> "module block"
    return $ ModuleBlock schema modules
