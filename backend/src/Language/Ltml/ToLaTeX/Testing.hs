{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Testing (parseTest)
where

import qualified Data.ByteString.Lazy as BSL
import Language.Ltml.ToLaTeX.PDFGenerator (generatePDF)
import Language.Ltml.Tree.Example.Fpo (fpoTree)
import Language.Ltml.Tree.Parser (TreeError (TreeError))
import Language.Ltml.Tree.ToLtml (treeToLtml)

parseTest :: IO ()
parseTest = do
    case treeToLtml fpoTree of
        Left (TreeError errMsg) -> putStrLn errMsg
        Right markedDocCon -> do
            eRes <- generatePDF markedDocCon
            case eRes of
                Left err -> putStrLn err
                Right pdf -> BSL.writeFile "./src/Language/Ltml/ToLaTeX/Auxiliary/out.pdf" pdf
