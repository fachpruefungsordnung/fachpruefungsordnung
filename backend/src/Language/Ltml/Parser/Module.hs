{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Module (moduleBlockP, moduleP) where

import qualified Data.Char as Char
import Data.Text (stripEnd)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Module
    ( CategoryType (..)
    , ModuleBlockType (..)
    , ModuleSchemaType (..)
    , ModuleType (..)
    )
import Language.Ltml.AST.Module
    ( Attribute (..)
    , Category (..)
    , Module (..)
    , ModuleBlock (..)
    , ModuleSchema (..)
    )
import Language.Ltml.Parser (MonadParser, Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Text.Megaparsec (many, sepBy1, takeWhile1P, (<?>))
import Text.Megaparsec.Char (char)

attributeListP :: (MonadParser m) => Keyword -> m [Attribute]
attributeListP kw = do
    nLexeme $ keywordP kw
    nLexeme attributeP `sepBy1` nLexeme (char ';')

-- | Permits ASCII Lower / Upper / Digit and special characters.
--   This is used for schema attributes AND module values of those attributes.
--
-- Currently the syntax is very free:
-- module:
-- A; 5
-- module: A
-- ;
--   5
attributeP :: (MonadParser m) => m Attribute
attributeP = Attribute . stripEnd <$> validP <?> "attribute"
  where
    -- TODO: parse TextTree, not raw Text
    --       also keyword ':' is currently not allowed in attributes
    isValid c =
        Char.isAsciiLower c
            || Char.isAsciiUpper c
            || Char.isDigit c
            || c `elem` "üöäÜÖÄ _-/,.!?\""
    validP = takeWhile1P (Just "attribute character") isValid

-------------------------------------------------------------------------------

schemaP :: (MonadParser m) => ModuleSchemaType -> m ModuleSchema
schemaP (ModuleSchemaType kw) = ModuleSchema <$> attributeListP kw <?> "module schema"

moduleP :: ModuleType -> Parser Module
moduleP (ModuleType kw) = Module <$> attributeListP kw <?> "module"

categoryP :: CategoryType -> Parser Category
categoryP (CategoryType kw moduleType) = do
    category <- attributeP
    nLexeme $ keywordP kw
    modules <- many (nLexeme (moduleP moduleType))
    return $ Category category modules

moduleBlockP
    :: ModuleBlockType -> Parser ModuleBlock
moduleBlockP (ModuleBlockType schemaType categoryType) = do
    schema <- nLexeme (schemaP schemaType)
    groups <- many $ nLexeme (categoryP categoryType)
    return $ ModuleBlock schema groups
