{-# LANGUAGE OverloadedStrings #-}
module Language.Ltml.ToLaTeX.TestAST 
    (testSection,
     readText,
     runTest)
where

import Text.Megaparsec
import Language.Lsd.Example.Fpo (superSectionT)
import Language.Ltml.Parser.Section (sectionP)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as TIO
import System.IO.Unsafe (unsafePerformIO)
import Language.Ltml.ToLaTeX.Type
import Language.Ltml.ToLaTeX.LabelState (LabelState (LabelState, labelToRef))
import Language.Ltml.ToLaTeXM (ToLaTeXM(toLaTeXM))
import Control.Monad.State (runState)
import Language.Ltml.ToLaTeX.Renderer (renderLaTeX)

readText :: String -> Text
readText filename = unsafePerformIO $ TIO.readFile filename

initialState :: LabelState
initialState = LabelState 0 0 0 False False mempty mempty

testSection :: (LaTeX, LabelState)
testSection = runState (
            toLaTeXM 
            $ either undefined id 
            $ runParser (sectionP superSectionT empty) "" (readText "./src/Language/Ltml/ToLaTeX/test.txt"))
            initialState

runTest :: IO ()
runTest = do
    let m = labelToRef (snd testSection)
        l = fst testSection
    TIO.putStrLn $ LT.toStrict $ renderLaTeX m l