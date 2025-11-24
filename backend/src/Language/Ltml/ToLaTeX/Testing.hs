{-# LANGUAGE OverloadedStrings #-}

-- | provides functions to render the example tree from Language.Ltml.Tree.Example.Fpo into pdf or latex-code
module Language.Ltml.ToLaTeX.Testing (parseTest, texTest, tableTest, moduleTest)
where

import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import Language.Lsd.AST.Common (Keyword (Keyword))
import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction))
import Language.Lsd.AST.Type.Module
    ( CategoryType (CategoryType)
    , ModuleBlockType (ModuleBlockType)
    , ModuleSchemaType (ModuleSchemaType)
    , ModuleType (ModuleType)
    )
import Language.Lsd.AST.Type.Text (TextType (TextType))
import Language.Ltml.AST.Module (ModuleBlock)
import Language.Ltml.Parser.Auxiliary.Test (table)
import Language.Ltml.Parser.Module (moduleBlockP)
import Language.Ltml.ToLaTeX.PDFGenerator (generateLaTeX, generatePDF)
import Language.Ltml.Tree.Example.Fpo (fpoTree)
import Language.Ltml.Tree.Parser (TreeError (TreeError))
import Language.Ltml.Tree.ToLtml (treeToLtml)
import Text.Megaparsec (errorBundlePretty, runParser)

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

tableTest :: IO ()
tableTest = do
    tbl <- table
    T.writeFile
        "./src/Language/Ltml/ToLaTeX/Auxiliary/table.tex"
        (generateLaTeX tbl)

moduleBlockT :: ModuleBlockType
moduleBlockT =
    ModuleBlockType
        (TextType (Disjunction []))
        (ModuleSchemaType (Keyword "schema:"))
        ( CategoryType
            (Keyword "category:")
            (ModuleType (Keyword "module:"))
        )

testParser :: Text -> IO ModuleBlock
testParser input = case runParser (moduleBlockP moduleBlockT) "" input of
    Left err -> fail $ errorBundlePretty err
    Right tbl -> do
        return tbl

modules :: IO ModuleBlock
modules = do
    -- input <- readFile "src/Language/Ltml/Parser/Auxiliary/test.txt"
    input <- readFile "./src/Language/Ltml/ToLaTeX/Auxiliary/module.example"
    testParser (pack input)

moduleTest :: IO ()
moduleTest = do
    modul <- modules
    T.writeFile
        "./src/Language/Ltml/ToLaTeX/Auxiliary/module.tex"
        (generateLaTeX modul)
