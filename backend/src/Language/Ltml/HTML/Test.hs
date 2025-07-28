{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Test () where

import Control.Applicative (Alternative (empty))
import Data.Text.IO.Utf8 (readFile)
import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Paragraph
import Language.Lsd.AST.Type.Section
import Language.Lsd.Example.Fpo (sectionT)
import Language.Ltml.AST.Label
import Language.Ltml.AST.Node
import Language.Ltml.AST.Paragraph
import Language.Ltml.AST.Section
import Language.Ltml.AST.Text
import Language.Ltml.Parser.Section (sectionP)
import Language.Ltml.HTML.HTML 

import Prelude hiding (Enum, Word, readFile)
import Language.Ltml.AST.Document (Document (..), DocumentHeader (..), DocumentBody (..))
import Language.Lsd.AST.Type.Document (DocumentFormat(..))
import Lucid (renderToFile)
import Text.Megaparsec (runParser)
import Language.Ltml.HTML.CSS.CSS (writeCss)

testSection :: Node Section
testSection =
    Node
        ( Just
            ( Label "sectiona"
            )
        )
        ( Section
            ( SectionFormat
                (FormatString [PlaceholderAtom Arabic])
            )
            ( Heading
                ( FormatString
                    [ StringAtom "§ "
                    , PlaceholderAtom IdentifierPlaceholder
                    , StringAtom ""
                    , PlaceholderAtom HeadingTextPlaceholder
                    ]
                )
                [ Word "Some"
                , Space
                , Word "section"
                ]
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
                , Node
                    Nothing
                    ( Paragraph
                        ( ParagraphFormat
                            (FormatString [PlaceholderAtom Arabic])
                        )
                        [ Special (SentenceStart Nothing)
                        , Word "This"
                        , Space
                        , Word "is"
                        , Space
                        , Word "another"
                        , Space
                        , Word "paragraph"
                        , Space
                        , Word "in"
                        , Space
                        , Reference
                            ( Label "sectiona"
                            )
                        , Word "."
                        , Space
                        , Special (SentenceStart Nothing)
                        , Word "Paragraphs"
                        , Space
                        , Word "don't"
                        , Space
                        , Word "have"
                        , Space
                        , Word "keywords"
                        , Space
                        , Word "and"
                        , Space
                        , Word "are"
                        , Space
                        , Word "just"
                        , Space
                        , Word "separated"
                        , Space
                        , Word "by"
                        , Space
                        , Word "empty"
                        , Space
                        , Word "lines"
                        , Word "."
                        , Enum
                            ( Enumeration
                                [ EnumItem
                                    [Word "First"]
                                , EnumItem
                                    [ Word "Second"
                                    , Enum
                                        ( Enumeration
                                            [ EnumItem
                                                [ Word "Sub"
                                                , Space
                                                , Word "One"
                                                ]
                                            , EnumItem
                                                [ Word "Sub"
                                                , Space
                                                , Word "Two"
                                                ]
                                            ]
                                        )
                                    ]
                                ]
                            )
                        ]
                    )
                ]
            )
        )

testDoc = readFile "src/Language/Ltml/HTML/Test/test.txt"

parseTest :: IO ()
parseTest = do
    text <- testDoc
    case runParser (sectionP sectionT empty) "" text of
        Left _ -> error "parsing failed"
        Right nodeSection -> do 
            renderToFile "src/Language/Ltml/HTML/Test/out.html" (sectionToHtml nodeSection)
            writeCss "src/Language/Ltml/HTML/Test/out.css"


-------------------------------------------------------------------------------

testAST :: Document
testAST =
    Document
        DocumentFormat
        DocumentHeader
        ( DocumentBody
            [ Node
                Nothing
                ( Section
                    (SectionFormat (FormatString []))
                    (Heading (FormatString []) [])
                    ( Left
                        [ Node
                            (Just (Label "label"))
                            ( Paragraph
                                (ParagraphFormat (FormatString []))
                                [ Word "Das ist im Paragraph"
                                , Word "Ohne Space"
                                , Space
                                , Word "Nach dem Space"
                                , Word "\n"
                                , Word "Neue Zeile?"
                                , Styled Bold [Word "Bold"]
                                , Styled Italics [Word "Italic"]
                                , Styled Underlined [Word "Underlined"]
                                ]
                            )
                        , Node
                            (Just (Label "Zweite Node Label"))
                            ( Paragraph
                                (ParagraphFormat (FormatString []))
                                [ Enum
                                    ( Enumeration
                                        [ EnumItem [Word "Erstes Item"]
                                        , EnumItem [Word "Zweites Item 1", Styled Bold [Word "Zweites Item 2"]]
                                        , EnumItem
                                            [ Enum
                                                ( Enumeration [EnumItem [Word "Zweite Aufzählung 1"], EnumItem [Word "Zweite 2"]]
                                                )
                                            ]
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                )
            ]
        )

testHtml :: IO ()
testHtml = renderToFile "static/out.html" (docToHtml testAST)
