{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Module (moduleBlockP, moduleP) where

import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Enum (EnumType)
import Language.Lsd.AST.Type.Module
    ( CategoryType (..)
    , ModuleBlockType (..)
    , ModuleSchemaType (..)
    , ModuleType (..)
    )
import Language.Lsd.AST.Type.Text (TextType)
import Language.Ltml.AST.Module
    ( Attribute (..)
    , Category (..)
    , Module (..)
    , ModuleBlock (..)
    , ModuleSchema (..)
    )
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (lexeme, nLexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.Text
import Text.Megaparsec (many, (<?>))

attributeListP :: Keyword -> Keyword -> TextType EnumType -> Parser [Attribute]
attributeListP kw sepKw tt = do
    lexeme $ keywordP kw
    fmap Attribute <$> seperatedTextForestsP sepKw tt

-------------------------------------------------------------------------------

schemaP
    :: ModuleSchemaType -> Keyword -> TextType EnumType -> Parser ModuleSchema
schemaP (ModuleSchemaType kw) sepKw tt = ModuleSchema <$> attributeListP kw sepKw tt <?> "module schema"

moduleP :: ModuleType -> Keyword -> TextType EnumType -> Parser Module
moduleP (ModuleType kw) sepKw tt = Module <$> attributeListP kw sepKw tt <?> "module"

categoryP :: CategoryType -> Keyword -> TextType EnumType -> Parser Category
categoryP (CategoryType kw moduleType) sepKw tt = do
    category <- Attribute <$> nLexeme (hangingTextP kw tt)
    modules <- many (nLexeme (moduleP moduleType sepKw tt))
    return $ Category category modules

-- | Parse a schema and 0 to n module definitions. The block is terminated by an empty line.
moduleBlockP
    :: ModuleBlockType -> Parser ModuleBlock
moduleBlockP (ModuleBlockType sepKw tt schemaType categoryType) = do
    schema <- lexeme (schemaP schemaType sepKw tt)
    groups <- many $ nLexeme $ categoryP categoryType sepKw tt
    return $ ModuleBlock schema groups
