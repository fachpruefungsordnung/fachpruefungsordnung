{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Module (moduleBlockP, moduleP) where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
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
    , ModuleTable (..)
    )
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (lexeme, nLexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.Text
    ( hangingTextP
    , pipeSeperatedTextForestsP
    )
import Text.Megaparsec (choice, many, some, (<?>))

attributeListP :: Keyword -> TextType Void -> Parser [Attribute]
attributeListP kw tt = do
    nLexeme $ keywordP kw
    fmap Attribute <$> pipeSeperatedTextForestsP tt

-------------------------------------------------------------------------------

schemaP
    :: ModuleSchemaType -> TextType Void -> Parser ModuleSchema
schemaP (ModuleSchemaType kw) tt = ModuleSchema <$> attributeListP kw tt <?> "module schema"

moduleP :: ModuleType -> TextType Void -> Parser Module
moduleP (ModuleType kw) tt = Module <$> attributeListP kw tt <?> "module"

categoryP :: CategoryType -> TextType Void -> Parser Category
categoryP (CategoryType kw moduleType) tt = do
    category <- Attribute <$> nLexeme (hangingTextP kw tt)
    modules <- many (nLexeme (moduleP moduleType tt))
    return $ Category category modules

-- | Parse a schema and 0 to n module definitions. The block is terminated by an empty line.
moduleBlockP
    :: ModuleBlockType -> Parser ModuleBlock
moduleBlockP (ModuleBlockType tt schemaType categoryType@(CategoryType _ moduleType)) = do
    schema <- lexeme (schemaP schemaType tt)
    groups <- choice moduleTablePs
    return $ ModuleBlock schema groups
  where
    moduleTablePs :: [Parser ModuleTable]
    moduleTablePs =
        -- If categorized failes (due to the 'some' requirement), we try plain.
        -- Thus, empty categorizeds are not allowed
        [ Categorized <$> some (nLexeme $ categoryP categoryType tt)
        , Plain <$> many (nLexeme $ moduleP moduleType tt)
        ]
