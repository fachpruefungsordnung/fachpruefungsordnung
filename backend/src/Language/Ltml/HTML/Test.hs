{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Test () where

import qualified Data.Map as Map
import Data.Text.IO.Utf8 (readFile)
import Data.Typography
    ( FontSize (..)
    , FontStyle (..)
    , TextAlignment (..)
    , Typography (..)
    )
import Language.Lsd.AST.Format
    ( EnumItemKeyFormat (..)
    , EnumStyle (..)
    , FormatAtom (..)
    , FormatString (..)
    , HeadingFormat (..)
    , HeadingPlaceholderAtom (..)
    , KeyPlaceholderAtom (..)
    , ParagraphKeyFormat (..)
    , TocKeyFormat (..)
    )
import Language.Lsd.AST.Type.AppendixSection
    ( AppendixElementFormat (..)
    , AppendixSectionFormat (..)
    , AppendixSectionTitle (..)
    )
import Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (DocumentContainerFormat)
    )
import Language.Lsd.AST.Type.Enum (EnumFormat (..), EnumItemFormat (..))
import Language.Lsd.AST.Type.Footnote (FootnoteFormat (..))
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (..))
import Language.Lsd.AST.Type.Section (SectionFormat (..))
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphFormat (..))
import Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionFormat (SimpleSectionFormat)
    )
import Language.Lsd.Example.Fpo
import Language.Ltml.AST.AppendixSection (AppendixSection (..))
import Language.Ltml.AST.Document
    ( Document (..)
    , DocumentBody (..)
    , DocumentHeading (DocumentHeading)
    )
import Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (DocumentContainer)
    , DocumentContainerHeader (DocumentContainerHeader)
    )
import Language.Ltml.AST.Footnote (Footnote (Footnote))
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.AST.Section (Heading (..), Section (..), SectionBody (..))
import Language.Ltml.AST.SimpleParagraph (SimpleParagraph (SimpleParagraph))
import Language.Ltml.AST.SimpleSection (SimpleSection (..))
import Language.Ltml.AST.Text
    ( EnumItem (..)
    , Enumeration (..)
    , FootnoteReference (..)
    , SentenceStart (..)
    , TextTree (..)
    )
import Language.Ltml.HTML
import Language.Ltml.HTML.CSS (writeCss)
import Language.Ltml.HTML.CSS.Util (addHtmlHeader)
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Parser.Footnote (unwrapFootnoteParser)
import Language.Ltml.Parser.Section (sectionP)
import Language.Ltml.Pretty (prettyPrint)
import Lucid (renderToFile)
import System.Directory (removeDirectoryRecursive)
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, runParser)
import Prelude hiding (Enum, Word, readFile)
import Language.Ltml.Tree.ToLtml (treeToLtml)
import Language.Ltml.Tree.Example.Fpo (fpoTree)
import Language.Ltml.Tree.Parser (TreeError(..))

testDoc = readFile "src/Language/Ltml/HTML/Test/test.txt"

parseTest :: IO ()
parseTest = do
    text <- testDoc
    case treeToLtml fpoTree of
        Left treeErr -> case treeErr of
            LeafError parseErr -> error $ errorBundlePretty parseErr
            TreeError strucErr -> putStrLn $ unlines strucErr
        Right markedDocCon -> do
            let (body, css) = renderHtmlCss markedDocCon
             in do
                    renderToFile
                        "src/Language/Ltml/HTML/Test/out.html"
                        (addHtmlHeader "" "out.css" body)
                    writeCss css "src/Language/Ltml/HTML/Test/out.css"

-------------------------------------------------------------------------------
{-
renderDocCon :: IO ()
renderDocCon =
    let (body, css) = renderHtmlCss documentContainer
     in do
            renderToFile
                "src/Language/Ltml/HTML/Test/out.html"
                (addHtmlHeader "" "out.css" body)
            writeCss css "src/Language/Ltml/HTML/Test/out.css"

-- Sample DocumentContainer

documentContainer :: DocumentContainer
documentContainer =
    DocumentContainer
        ( DocumentContainerFormat
            undefined
            undefined
            ( HeadingFormat
                (Typography Centered LargeFontSize [Bold])
                (FormatString [PlaceholderAtom HeadingTextPlaceholder])
            )
        )
        undefined
        document
        [appendixSection]

-- Sample Document

document :: Document
document =
    Document
        undefined
        (DocumentHeading [Word "Erstes", Word "Dokument"])
        ( DocumentBody
            [ SimpleSection
                (SimpleSectionFormat True)
                [ SimpleParagraph
                    (SimpleParagraphFormat $ Typography LeftAligned MediumFontSize [])
                    [Word "Intro:", Word "Paragraph", Space, Word "Text"]
                ]
            ]
            -- \^ intro
            (InnerSectionBody [superSection])
            -- \^ main
            []
            -- \^ outro
        )
        ( Map.insert
            (Label "f1")
            (Footnote SuperscriptFootnoteFormat [Word "Erste", Space, Word "Fußnote!"])
            Map.empty
        )

-- Sample AppendixSection

appendixSection :: AppendixSection
appendixSection =
    AppendixSection
        ( AppendixSectionFormat
            (AppendixSectionTitle "Anhang")
            ( AppendixElementFormat
                (FormatString [PlaceholderAtom Arabic])
                ( TocKeyFormat $
                    FormatString
                        [ StringAtom "Anlage "
                        , PlaceholderAtom KeyIdentifierPlaceholder
                        ]
                )
                ( HeadingFormat
                    (Typography LeftAligned LargeFontSize [Bold])
                    ( FormatString
                        [ StringAtom "Anlage "
                        , PlaceholderAtom IdentifierPlaceholder
                        , StringAtom "\n"
                        , PlaceholderAtom HeadingTextPlaceholder
                        ]
                    )
                )
            )
        )
        [Node (Just $ Label "anhang1") document, Node (Just $ Label "anhang2") document]

superSection :: Node Section
superSection =
    Node
        ( Just
            ( Label
                { unLabel = "main"
                }
            )
        )
        ( Section
            ( SectionFormat
                (FormatString [PlaceholderAtom Arabic])
                ( TocKeyFormat
                    ( FormatString
                        [ StringAtom "Abschnitt "
                        , PlaceholderAtom KeyIdentifierPlaceholder
                        ]
                    )
                )
            )
            ( Heading
                ( HeadingFormat
                    (Typography LeftAligned MediumFontSize [Bold])
                    ( FormatString
                        [ StringAtom "Abschnitt "
                        , PlaceholderAtom IdentifierPlaceholder
                        , StringAtom " "
                        , PlaceholderAtom HeadingTextPlaceholder
                        ]
                    )
                )
                [Word "Main"]
            )
            ( InnerSectionBody
                [ Node
                    ( Just
                        ( Label
                            { unLabel = "section_a"
                            }
                        )
                    )
                    ( Section
                        ( SectionFormat
                            (FormatString [PlaceholderAtom Arabic])
                            ( TocKeyFormat
                                ( FormatString
                                    [ StringAtom "§ "
                                    , PlaceholderAtom KeyIdentifierPlaceholder
                                    ]
                                )
                            )
                        )
                        ( Heading
                            ( HeadingFormat
                                (Typography Centered MediumFontSize [Bold])
                                ( FormatString
                                    [ StringAtom "§ "
                                    , PlaceholderAtom IdentifierPlaceholder
                                    , StringAtom "\n"
                                    , PlaceholderAtom HeadingTextPlaceholder
                                    ]
                                )
                            )
                            [ Word "Some"
                            , Space
                            , Word "section"
                            ]
                        )
                        ( LeafSectionBody
                            [ Node
                                Nothing
                                ( Paragraph
                                    ( ParagraphFormat
                                        (FormatString [PlaceholderAtom Arabic])
                                        ( ParagraphKeyFormat
                                            ( FormatString
                                                [ StringAtom "("
                                                , PlaceholderAtom KeyIdentifierPlaceholder
                                                , StringAtom ")"
                                                ]
                                            )
                                        )
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
                                        ( Label
                                            { unLabel = "section_a"
                                            }
                                        )
                                    , Space
                                    , Word "in"
                                    , Space
                                    , Word "super-section"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "main"
                                            }
                                        )
                                    , Word "."
                                    ]
                                )
                            , Node
                                Nothing
                                ( Paragraph
                                    ( ParagraphFormat
                                        (FormatString [PlaceholderAtom Arabic])
                                        ( ParagraphKeyFormat
                                            ( FormatString
                                                [ StringAtom "("
                                                , PlaceholderAtom KeyIdentifierPlaceholder
                                                , StringAtom ")"
                                                ]
                                            )
                                        )
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
                                        ( Label
                                            { unLabel = "section_a"
                                            }
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
                                    ]
                                )
                            ]
                        )
                    )
                , Node
                    ( Just
                        ( Label
                            { unLabel = "section_b"
                            }
                        )
                    )
                    ( Section
                        ( SectionFormat
                            (FormatString [PlaceholderAtom Arabic])
                            ( TocKeyFormat
                                ( FormatString
                                    [ StringAtom "§ "
                                    , PlaceholderAtom KeyIdentifierPlaceholder
                                    ]
                                )
                            )
                        )
                        ( Heading
                            ( HeadingFormat
                                (Typography Centered MediumFontSize [Bold])
                                ( FormatString
                                    [ StringAtom "§ "
                                    , PlaceholderAtom IdentifierPlaceholder
                                    , StringAtom "\n"
                                    , PlaceholderAtom HeadingTextPlaceholder
                                    ]
                                )
                            )
                            [ Word "Another"
                            , Space
                            , Word "section,"
                            , Space
                            , Word "with"
                            , Space
                            , Word "a"
                            , Space
                            , Word "title"
                            , Space
                            , Word "spanning"
                            , Space
                            , Word "several"
                            , Space
                            , Word "lines"
                            , Space
                            , FootnoteRef
                                ( FootnoteReference
                                    ( Label
                                        { unLabel = "f1"
                                        }
                                    )
                                )
                            ]
                        )
                        ( LeafSectionBody
                            [ Node
                                Nothing
                                ( Paragraph
                                    ( ParagraphFormat
                                        (FormatString [PlaceholderAtom Arabic])
                                        ( ParagraphKeyFormat
                                            ( FormatString
                                                [ StringAtom "("
                                                , PlaceholderAtom KeyIdentifierPlaceholder
                                                , StringAtom ")"
                                                ]
                                            )
                                        )
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
                                        ( Label
                                            { unLabel = "section_b"
                                            }
                                        )
                                    , Space
                                    , Word "in"
                                    , Space
                                    , Word "super-section"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "main"
                                            }
                                        )
                                    , Word "."
                                    ]
                                )
                            ]
                        )
                    )
                , Node
                    ( Just
                        ( Label
                            { unLabel = "sectiona"
                            }
                        )
                    )
                    ( Section
                        ( SectionFormat
                            (FormatString [PlaceholderAtom Arabic])
                            ( TocKeyFormat
                                ( FormatString
                                    [ StringAtom "§ "
                                    , PlaceholderAtom KeyIdentifierPlaceholder
                                    ]
                                )
                            )
                        )
                        ( Heading
                            ( HeadingFormat
                                (Typography Centered MediumFontSize [Bold])
                                ( FormatString
                                    [ StringAtom "§ "
                                    , PlaceholderAtom IdentifierPlaceholder
                                    , StringAtom "\n"
                                    , PlaceholderAtom HeadingTextPlaceholder
                                    ]
                                )
                            )
                            [ Word "Some"
                            , Space
                            , Word "section"
                            ]
                        )
                        ( LeafSectionBody
                            [ Node
                                Nothing
                                ( Paragraph
                                    ( ParagraphFormat
                                        (FormatString [PlaceholderAtom Arabic])
                                        ( ParagraphKeyFormat
                                            ( FormatString
                                                [ StringAtom "("
                                                , PlaceholderAtom KeyIdentifierPlaceholder
                                                , StringAtom ")"
                                                ]
                                            )
                                        )
                                    )
                                    [ Special (SentenceStart Nothing)
                                    , Word "And"
                                    , Space
                                    , Word "now"
                                    , Space
                                    , Word "i"
                                    , Space
                                    , Word "reference"
                                    , Space
                                    , Word "into"
                                    , Space
                                    , Word "the"
                                    , Space
                                    , Word "future"
                                    , Space
                                    , Word "to"
                                    , Space
                                    , Word "sentence"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "sen1"
                                            }
                                        )
                                    , Word "."
                                    ]
                                )
                            , Node
                                ( Just
                                    ( Label
                                        { unLabel = "para"
                                        }
                                    )
                                )
                                ( Paragraph
                                    ( ParagraphFormat
                                        (FormatString [PlaceholderAtom Arabic])
                                        ( ParagraphKeyFormat
                                            ( FormatString
                                                [ StringAtom "("
                                                , PlaceholderAtom KeyIdentifierPlaceholder
                                                , StringAtom ")"
                                                ]
                                            )
                                        )
                                    )
                                    [ Special (SentenceStart Nothing)
                                    , Word "This"
                                    , Space
                                    , Styled
                                        Underlined
                                        [Word "paragraph"]
                                    , Space
                                    , Word "is"
                                    , Space
                                    , Word "in"
                                    , Space
                                    , Word "section"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "sectiona"
                                            }
                                        )
                                    , Space
                                    , Word "in"
                                    , Space
                                    , Word "super-section"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "main"
                                            }
                                        )
                                    , Word "."
                                    , Space
                                    , Special
                                        ( SentenceStart
                                            ( Just
                                                ( Label
                                                    { unLabel = "sentencea"
                                                    }
                                                )
                                            )
                                        )
                                    , Word "This"
                                    , Space
                                    , Word "sentence"
                                    , Space
                                    , Word "is"
                                    , Space
                                    , Word "actually"
                                    , Space
                                    , Word "in"
                                    , Space
                                    , Word "§"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "para"
                                            }
                                        )
                                    , Space
                                    , Word "and"
                                    , Space
                                    , Word "this"
                                    , Space
                                    , Word "sentence"
                                    , Space
                                    , Word "is"
                                    , Space
                                    , Word "number"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "sentencea"
                                            }
                                        )
                                    , Word "."
                                    , Space
                                    , Special (SentenceStart Nothing)
                                    , Word "And"
                                    , Space
                                    , Word "it"
                                    , Space
                                    , Word "has"
                                    , Space
                                    , Word "a"
                                    , Space
                                    , Word "big"
                                    , Space
                                    , Word "Enumeration:"
                                    , Enum
                                        ( Enumeration
                                            ( EnumFormat
                                                ( EnumItemFormat
                                                    (FormatString [PlaceholderAtom Arabic])
                                                    ( EnumItemKeyFormat
                                                        ( FormatString
                                                            [ PlaceholderAtom KeyIdentifierPlaceholder
                                                            , StringAtom "."
                                                            ]
                                                        )
                                                    )
                                                )
                                            )
                                            [ Node
                                                ( Just
                                                    ( Label
                                                        { unLabel = "enum0"
                                                        }
                                                    )
                                                )
                                                ( EnumItem
                                                    [Word "First"]
                                                )
                                            , Node
                                                ( Just
                                                    ( Label
                                                        { unLabel = "enum1"
                                                        }
                                                    )
                                                )
                                                ( EnumItem
                                                    [ Word "Second"
                                                    , Enum
                                                        ( Enumeration
                                                            ( EnumFormat
                                                                ( EnumItemFormat
                                                                    (FormatString [PlaceholderAtom AlphabeticLower])
                                                                    ( EnumItemKeyFormat
                                                                        ( FormatString
                                                                            [ PlaceholderAtom KeyIdentifierPlaceholder
                                                                            , StringAtom ")"
                                                                            ]
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            [ Node
                                                                ( Just
                                                                    ( Label
                                                                        { unLabel = "enum2"
                                                                        }
                                                                    )
                                                                )
                                                                ( EnumItem
                                                                    [ Word "Sub"
                                                                    , Space
                                                                    , Word "One"
                                                                    ]
                                                                )
                                                            , Node
                                                                ( Just
                                                                    ( Label
                                                                        { unLabel = "enum3"
                                                                        }
                                                                    )
                                                                )
                                                                ( EnumItem
                                                                    [ Word "Sub"
                                                                    , Space
                                                                    , Word "Two"
                                                                    , Enum
                                                                        ( Enumeration
                                                                            ( EnumFormat
                                                                                ( EnumItemFormat
                                                                                    ( FormatString
                                                                                        [ PlaceholderAtom AlphabeticLower
                                                                                        , PlaceholderAtom AlphabeticLower
                                                                                        ]
                                                                                    )
                                                                                    ( EnumItemKeyFormat
                                                                                        ( FormatString
                                                                                            [ PlaceholderAtom KeyIdentifierPlaceholder
                                                                                            , StringAtom ")"
                                                                                            ]
                                                                                        )
                                                                                    )
                                                                                )
                                                                            )
                                                                            [ Node
                                                                                ( Just
                                                                                    ( Label
                                                                                        { unLabel = "enum4"
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( EnumItem
                                                                                    [Word "Third"]
                                                                                )
                                                                            , Node
                                                                                ( Just
                                                                                    ( Label
                                                                                        { unLabel = "enum5"
                                                                                        }
                                                                                    )
                                                                                )
                                                                                ( EnumItem
                                                                                    [ Word "Third"
                                                                                    , Space
                                                                                    , Word "Two"
                                                                                    , Enum
                                                                                        ( Enumeration
                                                                                            ( EnumFormat
                                                                                                ( EnumItemFormat
                                                                                                    ( FormatString
                                                                                                        [ PlaceholderAtom AlphabeticLower
                                                                                                        , PlaceholderAtom AlphabeticLower
                                                                                                        , PlaceholderAtom AlphabeticLower
                                                                                                        ]
                                                                                                    )
                                                                                                    ( EnumItemKeyFormat
                                                                                                        ( FormatString
                                                                                                            [ PlaceholderAtom KeyIdentifierPlaceholder
                                                                                                            , StringAtom ")"
                                                                                                            ]
                                                                                                        )
                                                                                                    )
                                                                                                )
                                                                                            )
                                                                                            [ Node
                                                                                                ( Just
                                                                                                    ( Label
                                                                                                        { unLabel = "enum6"
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                                ( EnumItem
                                                                                                    [ Word "Too"
                                                                                                    , Space
                                                                                                    , Word "much"
                                                                                                    ]
                                                                                                )
                                                                                            , Node
                                                                                                ( Just
                                                                                                    ( Label
                                                                                                        { unLabel = "enum7"
                                                                                                        }
                                                                                                    )
                                                                                                )
                                                                                                ( EnumItem
                                                                                                    [ Word "Way"
                                                                                                    , Space
                                                                                                    , Word "too"
                                                                                                    , Space
                                                                                                    , Word "much"
                                                                                                    , Enum
                                                                                                        ( Enumeration
                                                                                                            ( EnumFormat
                                                                                                                ( EnumItemFormat
                                                                                                                    (FormatString [PlaceholderAtom Arabic])
                                                                                                                    ( EnumItemKeyFormat
                                                                                                                        ( FormatString
                                                                                                                            [StringAtom "-"]
                                                                                                                        )
                                                                                                                    )
                                                                                                                )
                                                                                                            )
                                                                                                            [ Node
                                                                                                                Nothing
                                                                                                                ( EnumItem
                                                                                                                    [Word "Fail?"]
                                                                                                                )
                                                                                                            ]
                                                                                                        )
                                                                                                    ]
                                                                                                )
                                                                                            ]
                                                                                        )
                                                                                    ]
                                                                                )
                                                                            ]
                                                                        )
                                                                    ]
                                                                )
                                                            ]
                                                        )
                                                    ]
                                                )
                                            ]
                                        )
                                    , Special (SentenceStart Nothing)
                                    , Word "Sentence"
                                    , Space
                                    , Word "after"
                                    , Space
                                    , Word "enum"
                                    , Word "."
                                    ]
                                )
                            , Node
                                Nothing
                                ( Paragraph
                                    ( ParagraphFormat
                                        (FormatString [PlaceholderAtom Arabic])
                                        ( ParagraphKeyFormat
                                            ( FormatString
                                                [ StringAtom "("
                                                , PlaceholderAtom KeyIdentifierPlaceholder
                                                , StringAtom ")"
                                                ]
                                            )
                                        )
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
                                    , Word "§"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "sectiona"
                                            }
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
                                    , Space
                                    , Special (SentenceStart Nothing)
                                    , Word "But"
                                    , Space
                                    , Word "whats"
                                    , Space
                                    , Word "about"
                                    , Space
                                    , Word "references"
                                    , Space
                                    , Word "that"
                                    , Space
                                    , Word "are"
                                    , Space
                                    , Word "into"
                                    , Space
                                    , Word "the"
                                    , Space
                                    , Word "future"
                                    , Space
                                    , Word "to"
                                    , Space
                                    , Word "§"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "sectionb"
                                            }
                                        )
                                    , Word "?"
                                    , Space
                                    , Special (SentenceStart Nothing)
                                    , Word "Testing"
                                    , Space
                                    , Word "references"
                                    , Space
                                    , Word "that"
                                    , Space
                                    , Word "dont"
                                    , Space
                                    , Word "even"
                                    , Space
                                    , Word "exist"
                                    , Space
                                    , Word "at"
                                    , Space
                                    , Word "all"
                                    , Space
                                    , Word "like:"
                                    , Space
                                    , Word "§"
                                    , Space
                                    , Reference
                                        ( Label
                                            { unLabel = "sectionc"
                                            }
                                        )
                                    , Word "."
                                    ]
                                )
                            ]
                        )
                    )
                ]
            )
        )

-------------------------------------------------------------------------------

-- exportTest :: IO ()
-- exportTest =
--     let testDir = "src/Language/Ltml/HTML/Test/Doc"
--      in do
--             text <- testDoc
--             case runParser (sectionP superSectionT eof) "" text of
--                 Left _ -> error "parsing failed"
--                 Right nodeSection -> do
--                     exportDocument
--                         ( Document
--                             DocumentFormat
--                             (DocumentTitle "Titel")
--                             (DocumentBody [nodeSection, nodeSection])
--                         )
--                         testDir
--             _ <- getLine
--             removeDirectoryRecursive testDir

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

-}
