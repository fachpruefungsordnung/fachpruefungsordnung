{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Label
    ( labelP
    , labelingP
    , bracedLabelingP
    )
where

import qualified Data.Char as Char (isAsciiLower, isDigit)
import qualified Data.Text as Text (cons)
import Language.Ltml.AST.Label (Label (Label))
import Language.Ltml.Parser (MonadParser)
import Text.Megaparsec (satisfy, takeWhileP, (<?>))
import Text.Megaparsec.Char (char)

labelP :: (MonadParser m) => m Label
labelP = Label <$> (Text.cons <$> headP <*> tailP) <?> "label"
  where
    isFirst = Char.isAsciiLower
    isLater c = Char.isAsciiLower c || Char.isDigit c || c `elem` "_-"
    headP = satisfy isFirst
    tailP = takeWhileP (Just "label character") isLater

labelingP :: (MonadParser m) => m Label
labelingP = char '{' *> bracedLabelingP <* char '}'

bracedLabelingP :: (MonadParser m) => m Label
bracedLabelingP = labelP <* char ':'
