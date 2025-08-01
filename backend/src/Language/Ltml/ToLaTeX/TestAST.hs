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
import Language.Ltml.ToLaTeX.Type
import Language.Ltml.ToLaTeX.LabelState (LabelState (LabelState))
import Language.Ltml.ToLaTeXM (ToLaTeXM(toLaTeXM))
import Control.Monad.State (evalState)

readText :: String -> Text
readText filename = unsafePerformIO $ TIO.readFile filename

initialState :: LabelState
initialState = LabelState 0 0 0 False False

testSection :: LaTeX
testSection = evalState (
            toLaTeXM 
            $ either undefined id 
            $ runParser (sectionP superSectionT empty) "" (readText "./src/Language/Ltml/ToLaTeX/test.txt"))
            initialState