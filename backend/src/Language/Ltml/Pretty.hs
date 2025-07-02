{-# LANGUAGE ConstraintKinds #-}

module Language.Ltml.Pretty
    ( prettyPrint
    , prettyParseTest
    , prettyErrorParse
    )
where

import Data.Text (Text)
import Language.Ltml.Parser (Parser)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.Pretty.Simple (pPrint)

type Pretty = Show

prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = pPrint

prettyParseTest :: (Pretty a) => Parser a -> Text -> IO ()
prettyParseTest p input = prettyErrorParse p input >>= prettyPrint

prettyErrorParse :: Parser a -> Text -> IO a
prettyErrorParse p =
    either (\e -> hPutStr stderr (errorBundlePretty e) >> exitFailure) return
        . runParser p ""
