{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Test () where

import Data.ByteString.Lazy (writeFile)
import Language.Ltml.HTML
import Language.Ltml.HTML.CSS (writeCss)
import Language.Ltml.HTML.CSS.Util (addHtmlHeader)
import Language.Ltml.HTML.Export (renderZip)
import Language.Ltml.Tree.Example.Fpo (fpoTree)
import Language.Ltml.Tree.Parser (TreeError (..))
import Language.Ltml.Tree.ToLtml (treeToLtml)
import Lucid (renderToFile)
import System.Directory (removeDirectoryRecursive)
import Prelude hiding (writeFile)

parseTest :: IO ()
parseTest = do
    case treeToLtml fpoTree of
        Left (TreeError errMsg) -> putStrLn errMsg
        Right markedDocCon -> do
            let (body, css) = renderHtmlCss markedDocCon
             in do
                    mapM_ print $ renderTocList markedDocCon
                    renderToFile
                        "src/Language/Ltml/HTML/Test/out.html"
                        (addHtmlHeader ("Generated Document Preview" :: String) "out.css" body)
                    writeCss css "src/Language/Ltml/HTML/Test/out.css"

-- prettyPrint markedDocCon

-------------------------------------------------------------------------------

-- exportTest :: IO ()
-- exportTest =
--     let testDir = "src/Language/Ltml/HTML/Test/export"
--      in do
--             _ <- case treeToLtml fpoTree of
--                 Left _ -> error "parsing failed"
--                 Right docCon -> exportDocument docCon testDir
--             _ <- getLine
--             removeDirectoryRecursive testDir

zipTest :: IO ()
zipTest = do
    case treeToLtml fpoTree of
        Left (TreeError errMsg) -> error errMsg
        Right docCon -> do
            mBs <- renderZip docCon
            case mBs of
                Nothing -> putStrLn "AST has errors! No ZIP"
                Just bs -> writeFile "src/Language/Ltml/HTML/Test/export.zip" bs

-------------------------------------------------------------------------------
