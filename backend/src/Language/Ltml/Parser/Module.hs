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
import Language.Lsd.AST.Type.Table (CellFormat)
import Language.Lsd.AST.Type.Text (TextType)
import Language.Ltml.AST.Table (Cell (..), Row (..), Table (..))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (lexeme, nLexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.Text
    ( hangingTextP
    , pipeSeperatedTextForestsP
    )
import Text.Megaparsec (choice, many, some, try, (<?>))

attributeListP :: Keyword -> CellFormat -> TextType Void -> Parser [Cell]
attributeListP kw cf tt = do
    nLexeme $ keywordP kw
    fmap (\tf -> Cell cf tf 1 1) <$> pipeSeperatedTextForestsP tt

-------------------------------------------------------------------------------

schemaP
    :: ModuleSchemaType -> TextType Void -> Parser Row
schemaP (ModuleSchemaType kw cf) tt = Row <$> attributeListP kw cf tt <?> "module schema"

moduleP :: ModuleType -> TextType Void -> Parser [Cell]
moduleP (ModuleType kw cf) tt = attributeListP kw cf tt <?> "module"

categoryP :: CategoryType -> TextType Void -> Parser [Row]
categoryP (CategoryType kw cf moduleType) tt = do
    categoryTree <- nLexeme (hangingTextP kw tt)
    modules <- many (nLexeme (moduleP moduleType tt))
    return $ case modules of
        [] -> [Row [Cell cf categoryTree 1 1]]
        (m : ms) -> Row (Cell cf categoryTree 1 (length modules) : m) : fmap Row ms

-- | Parse a schema and 0 to n module definitions. The block is terminated by an empty line.
moduleBlockP
    :: ModuleBlockType -> Parser Table
moduleBlockP
    ( ModuleBlockType
            tt
            schemaType@(ModuleSchemaType _ schemaCellFormat)
            categoryType@(CategoryType _ _ moduleType)
        ) = do
        -- categorized has to be tried first (see below)!
        Table <$> choice [try categorizedP, plainP]
      where
        -- If categorized failes (due to the 'some' requirement), we try plain.
        -- Thus, empty categorizeds are not allowed
        categorizedP :: Parser [Row]
        categorizedP = do
            (Row schemaCells) <- lexSchemaP
            categoryRows <- concat <$> some (nLexeme $ categoryP categoryType tt)
            return $ Row (Cell schemaCellFormat [] 1 1 : schemaCells) : categoryRows

        plainP :: Parser [Row]
        plainP = do
            schemaRow <- lexSchemaP
            groups <- fmap Row <$> many (nLexeme $ moduleP moduleType tt)
            return $ schemaRow : groups

        lexSchemaP = lexeme (schemaP schemaType tt)
