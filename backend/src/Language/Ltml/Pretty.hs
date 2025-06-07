{-# LANGUAGE ConstraintKinds #-}

module Language.Ltml.Pretty
    ( prettyPrint
    , prettyParseTest
    )
where

import Data.Text (Text)
import Language.Ltml.Parser (Parser)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.Pretty.Simple (pPrint)

type Pretty = Show

prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = pPrint

prettyParseTest :: (Pretty a) => Parser a -> Text -> IO ()
prettyParseTest p =
    either (putStr . errorBundlePretty) pPrint
        . runParser p ""
