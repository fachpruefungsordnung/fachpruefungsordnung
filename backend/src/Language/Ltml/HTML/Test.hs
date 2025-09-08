{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Test () where

import Language.Ltml.HTML
import Language.Ltml.HTML.CSS (writeCss)
import Language.Ltml.HTML.CSS.Util (addHtmlHeader)
import Language.Ltml.Pretty (prettyPrint)
import Language.Ltml.Tree.Example.Fpo (fpoTree)
import Language.Ltml.Tree.Parser (TreeError (..))
import Language.Ltml.Tree.ToLtml (treeToLtml)
import Lucid (renderToFile)
import System.Directory (removeDirectoryRecursive)
import Prelude hiding (Enum, Word, readFile)
import Language.Ltml.HTML.Export (exportDocument)

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
                        (addHtmlHeader "Generated Document Preview" "out.css" body)
                    writeCss css "src/Language/Ltml/HTML/Test/out.css"

-- prettyPrint markedDocCon

-------------------------------------------------------------------------------

exportTest :: IO ()
exportTest =
    let testDir = "src/Language/Ltml/HTML/Test/export"
     in do
        _ <- case treeToLtml fpoTree of
            Left _ -> error "parsing failed"
            Right docCon -> exportDocument docCon testDir
        _ <- getLine
        removeDirectoryRecursive testDir

-------------------------------------------------------------------------------

-- replicateSection :: Node Section
-- replicateSection =
--     Node Nothing $
--         Section
--             ( SectionFormat
--                 (FormatString [PlaceholderAtom Arabic])
--                 ( TocKeyFormat $
--                     FormatString [StringAtom "§ ", PlaceholderAtom KeyIdentifierPlaceholder]
--                 )
--             )
--             ( Heading
--                 (FormatString [StringAtom "§ ", PlaceholderAtom IdentifierPlaceholder])
--                 []
--             )
--             ( Left
--                 [ Node
--                     Nothing
--                     ( Paragraph
--                         ( ParagraphFormat
--                             (FormatString [PlaceholderAtom Arabic])
--                             ( ParagraphKeyFormat $
--                                 FormatString
--                                     [StringAtom "(", PlaceholderAtom KeyIdentifierPlaceholder, StringAtom ")"]
--                             )
--                         )
--                         [ Special (SentenceStart Nothing)
--                         , Word "This"
--                         , Space
--                         , Word "paragraph"
--                         , Space
--                         , Word "is"
--                         , Space
--                         , Word "in"
--                         , Space
--                         , Reference
--                             ( Label "sectiona"
--                             )
--                         , Space
--                         , Word "in"
--                         , Space
--                         , Word "super-section"
--                         , Space
--                         , Reference
--                             ( Label "main"
--                             )
--                         , Word "."
--                         ]
--                     )
--                 ]
--             )

-- scalableSection :: Int -> IO ()
-- scalableSection n = do
--     -- TODO: has to build final css from rendering
--     -- writeCss "src/Language/Ltml/HTML/Test/out.css"
--     renderToFile "src/Language/Ltml/HTML/Test/out.html" $
--         sectionToHtml
--             ( Node (Just (Label "main")) $
--                 Section
--                     ( SectionFormat
--                         (FormatString [PlaceholderAtom Arabic])
--                         ( TocKeyFormat $
--                             FormatString [StringAtom "§ ", PlaceholderAtom KeyIdentifierPlaceholder]
--                         )
--                     )
--                     ( Heading
--                         (FormatString [StringAtom "Abschnitt ", PlaceholderAtom IdentifierPlaceholder])
--                         []
--                     )
--                     (Right (replicate n replicateSection))
--             )

-------------------------------------------------------------------------------
