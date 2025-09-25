{-# LANGUAGE OverloadedStrings #-}

-- | provides functions to render the example tree from Language.Ltml.Tree.Example.Fpo into pdf or latex-code
module Language.Ltml.ToLaTeX.Testing (parseTest, texTest)
where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO as T
import Language.Ltml.ToLaTeX.PDFGenerator (generateLaTeX, generatePDF)
import Language.Ltml.Tree.Example.Fpo (fpoTree)
import Language.Ltml.Tree.Parser (TreeError (TreeError))
import Language.Ltml.Tree.ToLtml (treeToLtml)

-- | renders the fpoTree into a pdf (or fails)
parseTest :: IO ()
parseTest = do
    case treeToLtml fpoTree of
        Left (TreeError errMsg) -> putStrLn errMsg
        Right markedDocCon -> do
            eRes <- generatePDF markedDocCon
            case eRes of
                Left err -> putStrLn err
                Right pdf -> BSL.writeFile "./src/Language/Ltml/ToLaTeX/Auxiliary/out.pdf" pdf

-- | renders the fpoTree into latex code (or fails)
texTest :: IO ()
texTest = do
    case treeToLtml fpoTree of
        Left (TreeError errMsg) -> putStrLn errMsg
        Right markedDocCon -> do
            T.writeFile
                "./src/Language/Ltml/ToLaTeX/Auxiliary/out.tex"
                (generateLaTeX markedDocCon)
