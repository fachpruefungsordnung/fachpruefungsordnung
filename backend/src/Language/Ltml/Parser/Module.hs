{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Module (schemaP) where

import qualified Data.Char as Char
import qualified Data.Text as Text
import Language.Lsd.AST.Type.Module (SchemaType (SchemaType))
import Language.Ltml.AST.Module (Attribute (..), Schema (..))
import Language.Ltml.Parser (MonadParser)
import Language.Ltml.Parser.Common.Lexeme (lexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Text.Megaparsec (satisfy, sepBy, takeWhileP, (<?>))
import Text.Megaparsec.Char (char)

schemaP :: (MonadParser m) => SchemaType -> m Schema
schemaP (SchemaType kw) = do
    lexeme $ keywordP kw
    Schema <$> lexeme attributeP `sepBy` lexeme (char ',')

attributeP :: (MonadParser m) => m Attribute
attributeP = Attribute <$> (Text.cons <$> headP <*> tailP) <?> "attribute"
  where
    isFirst c = Char.isAsciiUpper c || Char.isAsciiLower c || c `elem` "_-"
    isLater c =
        Char.isAsciiLower c
            || Char.isAsciiUpper c
            || Char.isDigit c
            || c `elem` "_-"
    headP = satisfy isFirst
    tailP = takeWhileP (Just "attribute character") isLater
