{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Label
    ( labelP
    )
where

import Control.Applicative.Utils ((<:>))
import qualified Data.Char as Char (isAsciiLower, isDigit)
import qualified Data.Text as Text (unpack)
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.Parser (MonadParser)
import Text.Megaparsec (satisfy, takeWhileP, (<?>))

labelP :: (MonadParser m) => m Label
labelP = Label <$> (headP <:> tailP) <?> "label"
  where
    isFirst = Char.isAsciiLower
    isLater c = Char.isAsciiLower c || Char.isDigit c || c `elem` "_-:"
    headP = satisfy isFirst
    tailP = Text.unpack <$> takeWhileP (Just "label character") isLater
