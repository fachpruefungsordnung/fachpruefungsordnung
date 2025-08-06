{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Test () where

import Control.Applicative (Alternative (empty))
import Data.Text.IO.Utf8 (readFile)
import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Paragraph
import Language.Lsd.AST.Type.Section
import Language.Lsd.Example.Fpo
import Language.Ltml.AST.Label
import Language.Ltml.AST.Node
import Language.Ltml.AST.Paragraph
import Language.Ltml.AST.Section
import Language.Ltml.AST.Text
import Language.Ltml.HTML.Export
import Language.Ltml.HTML.HTML
import Language.Ltml.Parser.Section (sectionP)

import Language.Lsd.AST.Type.Document (DocumentFormat (..))
import Language.Ltml.AST.Document
    ( Document (..)
    , DocumentBody (..)
    , DocumentHeader (..)
    )
import Language.Ltml.HTML.CSS.CSS (writeCss)
import Language.Ltml.Pretty (prettyPrint)
import Lucid (renderToFile)
import System.Directory (removeDirectoryRecursive)
import Text.Megaparsec (runParser)
import Prelude hiding (Enum, Word, readFile)

testDoc = readFile "src/Language/Ltml/HTML/Test/studienaufbau_master.txt"

parseTest :: IO ()
parseTest = do
    text <- testDoc
    case runParser (sectionP sectionT empty) "" text of
        Left err -> error $ show err
        Right nodeSection -> do
            renderToFile "src/Language/Ltml/HTML/Test/out.html" (sectionToHtml nodeSection)
            writeCss "src/Language/Ltml/HTML/Test/out.css"
            prettyPrint nodeSection

-------------------------------------------------------------------------------

exportTest :: IO ()
exportTest =
    let testDir = "src/Language/Ltml/HTML/Test/Doc"
     in do
            text <- testDoc
            case runParser (sectionP superSectionT empty) "" text of
                Left _ -> error "parsing failed"
                Right nodeSection -> do
                    exportDocument
                        ( Document
                            DocumentFormat
                            DocumentHeader
                            (DocumentBody [nodeSection, nodeSection])
                        )
                        testDir
            _ <- getLine
            removeDirectoryRecursive testDir

-------------------------------------------------------------------------------

replicateSection :: Node Section
replicateSection =
    Node Nothing $
        Section
            (SectionFormat (FormatString [PlaceholderAtom Arabic]))
            ( Heading
                (FormatString [StringAtom "§ ", PlaceholderAtom IdentifierPlaceholder])
                []
            )
            ( Left
                [ Node
                    Nothing
                    ( Paragraph
                        ( ParagraphFormat
                            (FormatString [PlaceholderAtom Arabic])
                        )
                        [ Special (SentenceStart Nothing)
                        , Word "This"
                        , Space
                        , Word "paragraph"
                        , Space
                        , Word "is"
                        , Space
                        , Word "in"
                        , Space
                        , Reference
                            ( Label "sectiona"
                            )
                        , Space
                        , Word "in"
                        , Space
                        , Word "super-section"
                        , Space
                        , Reference
                            ( Label "main"
                            )
                        , Word "."
                        ]
                    )
                ]
            )

scalableSection :: Int -> IO ()
scalableSection n = do
    writeCss "src/Language/Ltml/HTML/Test/out.css"
    renderToFile "src/Language/Ltml/HTML/Test/out.html" $
        sectionToHtml
            ( Node (Just (Label "main")) $
                Section
                    (SectionFormat (FormatString [PlaceholderAtom Arabic]))
                    ( Heading
                        (FormatString [StringAtom "Abschnitt ", PlaceholderAtom IdentifierPlaceholder])
                        []
                    )
                    (Right (replicate n replicateSection))
            )

-------------------------------------------------------------------------------
