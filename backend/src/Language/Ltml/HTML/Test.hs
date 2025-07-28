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
import Language.Ltml.Pretty (prettyParseTest)
import Language.Ltml.HTML.HTML 

import Prelude hiding (Enum, Word, readFile)

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

testDoc = readFile "src/Language/Ltml/HTML/test.txt"

parseTest = do
    text <- testDoc
    prettyParseTest (sectionP sectionT empty) text
