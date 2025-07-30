{-# LANGUAGE OverloadedStrings #-}
module Language.Ltml.ToLaTeX.TestAST 
    (testSection,
     readText)
where

import Text.Megaparsec
import Language.Lsd.Example.Fpo (superSectionT)
import Language.Ltml.Parser.Section (sectionP)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO.Unsafe (unsafePerformIO)
import Language.Ltml.ToLaTeX (ToLaTeX(toLaTeX))
import Language.Ltml.ToLaTeX.Type

readText :: String -> Text
readText filename = unsafePerformIO $ TIO.readFile filename

testSection :: LaTeX
testSection = toLaTeX $ either undefined id $ runParser (sectionP superSectionT empty) "" (readText "./src/Language/Ltml/ToLaTeX/test.txt")