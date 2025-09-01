{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Test () where

import Data.Map (fromList)
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
import Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , TocFormat (..)
    , TocHeading (..)
    )
import Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (..)
    , HeaderFooterFormat (..)
    , HeaderFooterFormatAtom (..)
    , HeaderFooterItemFormat (..)
    )
import Language.Lsd.AST.Type.Enum (EnumFormat (..), EnumItemFormat (..))
import Language.Lsd.AST.Type.Footnote (FootnoteFormat (..))
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (..))
import Language.Lsd.AST.Type.Section (SectionFormat (..))
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphFormat (..))
import Language.Lsd.AST.Type.SimpleSection (SimpleSectionFormat (..))
import Language.Ltml.AST.AppendixSection (AppendixSection (..))
import Language.Ltml.AST.Document
    ( Document (..)
    , DocumentBody (..)
    , DocumentHeading (..)
    )
import Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (..)
    , DocumentContainerHeader (..)
    )
import Language.Ltml.AST.Footnote (Footnote (..))
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.AST.Section (Heading (..), Section (..), SectionBody (..))
import Language.Ltml.AST.SimpleParagraph (SimpleParagraph (..))
import Language.Ltml.AST.SimpleSection (SimpleSection (..))
import Language.Ltml.AST.Text
    ( EnumItem (..)
    , Enumeration (..)
    , FootnoteReference (..)
    , SentenceStart (..)
    , TextTree (..)
    )
import Language.Ltml.Common (Flagged (..), Flagged')
import Language.Ltml.HTML
import Language.Ltml.HTML.CSS (writeCss)
import Language.Ltml.HTML.CSS.Util (addHtmlHeader)
import Language.Ltml.Pretty (prettyPrint)
import Language.Ltml.Tree.Example.Fpo (fpoTree)
import Language.Ltml.Tree.Parser (TreeError (..))
import Language.Ltml.Tree.ToLtml (treeToLtml)
import Lucid (renderToFile)
import System.Directory (removeDirectoryRecursive)
import Text.Megaparsec (errorBundlePretty)
import Prelude hiding (Enum, Word, readFile)

parseTest :: IO ()
parseTest = do
    case treeToLtml fpoTree of
        Left treeErr -> case treeErr of
            LeafError parseErr -> error $ errorBundlePretty parseErr
            TreeError strucErr -> putStrLn $ unlines strucErr
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

docConTest :: Flagged' DocumentContainer
docConTest =
    Flagged
        False
        ( DocumentContainer
            ( DocumentContainerFormat
                ( HeaderFooterFormat
                    [ HeaderFooterItemFormat
                        MediumFontSize
                        [Bold]
                        ( FormatString
                            [ PlaceholderAtom HeaderFooterSuperTitleAtom
                            , StringAtom "\n"
                            ]
                        )
                    , HeaderFooterItemFormat
                        MediumFontSize
                        []
                        (FormatString [PlaceholderAtom HeaderFooterTitleAtom])
                    ]
                    []
                    [ HeaderFooterItemFormat
                        SmallFontSize
                        []
                        ( FormatString
                            [StringAtom "(Keine amtliche Bekanntmachung)"]
                        )
                    ]
                )
                ( HeaderFooterFormat
                    [ HeaderFooterItemFormat
                        SmallFontSize
                        []
                        ( FormatString
                            [ StringAtom "Stand: "
                            , PlaceholderAtom HeaderFooterDateAtom
                            ]
                        )
                    ]
                    []
                    [ HeaderFooterItemFormat
                        SmallFontSize
                        []
                        ( FormatString
                            [ StringAtom "Seite "
                            , PlaceholderAtom HeaderFooterCurPageNumAtom
                            , StringAtom " / "
                            , PlaceholderAtom HeaderFooterLastPageNumAtom
                            ]
                        )
                    ]
                )
                ( HeadingFormat
                    (Typography Centered LargeFontSize [Bold])
                    (FormatString [PlaceholderAtom HeadingTextPlaceholder])
                )
            )
            ( DocumentContainerHeader
                { dchPdfTitle = "Beispieltitel"
                , dchHeaderFooterSuperTitle = "Universität BeispielStadt"
                , dchHeaderFooterTitle = "FPO Beispiel 2025"
                , dchHeaderFooterDate = "2025-08-26"
                }
            )
            ( Flagged
                False
                ( Document
                    (DocumentFormat (Just (TocFormat (TocHeading "Inhaltsverzeichnis"))))
                    ( DocumentHeading
                        [Word "Beispiel-Überschrift"]
                    )
                    ( DocumentBody
                        ( Flagged
                            False
                            [ SimpleSection
                                (SimpleSectionFormat {ssHasPrecedingHorizontalBar = False})
                                [ SimpleParagraph
                                    ( SimpleParagraphFormat
                                        (Typography Centered LargeFontSize [])
                                    )
                                    [ Word "Vom"
                                    , Space
                                    , Word "19."
                                    , Space
                                    , Word "Januar"
                                    , Space
                                    , Word "2038"
                                    ]
                                ]
                            , SimpleSection
                                (SimpleSectionFormat {ssHasPrecedingHorizontalBar = False})
                                []
                            , SimpleSection
                                (SimpleSectionFormat {ssHasPrecedingHorizontalBar = False})
                                []
                            ]
                        )
                        ( Flagged
                            False
                            ( InnerSectionBody
                                [ Flagged
                                    True
                                    ( Node
                                        Nothing
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
                                                [ Word "section"
                                                , Space
                                                , Word "title"
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
                                                        , Word "is"
                                                        , Space
                                                        , Word "a"
                                                        , Space
                                                        , Word "simple"
                                                        , Space
                                                        , Word "paragraph"
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
                                                        , Word "paragraph,"
                                                        , Space
                                                        , Word "with"
                                                        , Space
                                                        , Word "an"
                                                        , Space
                                                        , Word "enumeration:"
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
                                                                    Nothing
                                                                    ( EnumItem
                                                                        [ Word "first"
                                                                        , Space
                                                                        , Word "item"
                                                                        ]
                                                                    )
                                                                , Node
                                                                    Nothing
                                                                    ( EnumItem
                                                                        [ Word "second"
                                                                        , Space
                                                                        , Word "item"
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
                                                                                    Nothing
                                                                                    ( EnumItem
                                                                                        [Word "sub-item"]
                                                                                    )
                                                                                ]
                                                                            )
                                                                        ]
                                                                    )
                                                                , Node
                                                                    Nothing
                                                                    ( EnumItem
                                                                        [ Word "third"
                                                                        , Space
                                                                        , Word "item,"
                                                                        , Space
                                                                        , Word "which"
                                                                        , Space
                                                                        , Word "spans"
                                                                        , Space
                                                                        , Word "several"
                                                                        , Space
                                                                        , Word "lines"
                                                                        ]
                                                                    )
                                                                ]
                                                            )
                                                        ]
                                                    )
                                                , Node
                                                    ( Just
                                                        ( Label
                                                            { unLabel = "some_paragraph"
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
                                                        , Word "A"
                                                        , Space
                                                        , Word "labeled"
                                                        , Space
                                                        , Word "paragraph,"
                                                        , Space
                                                        , Word "referencing"
                                                        , Space
                                                        , Word "itself:"
                                                        , Space
                                                        , Reference
                                                            ( Label
                                                                { unLabel = "some_paragraph"
                                                                }
                                                            )
                                                        , Word "."
                                                        , Space
                                                        , Special (SentenceStart Nothing)
                                                        , Word "It"
                                                        , Space
                                                        , Word "also"
                                                        , Space
                                                        , Word "has"
                                                        , Space
                                                        , Word "a"
                                                        , Space
                                                        , Word "footnote"
                                                        , FootnoteRef
                                                            ( FootnoteReference
                                                                ( Label
                                                                    { unLabel = "a_footnote"
                                                                    }
                                                                )
                                                            )
                                                        , Word "."
                                                        , Space
                                                        , Special
                                                            ( SentenceStart
                                                                ( Just
                                                                    ( Label
                                                                        { unLabel = "a_sentence"
                                                                        }
                                                                    )
                                                                )
                                                            )
                                                        , Word "It"
                                                        , Space
                                                        , Word "also"
                                                        , Space
                                                        , Word "has"
                                                        , Space
                                                        , Word "a"
                                                        , Space
                                                        , Word "self-referencing"
                                                        , Space
                                                        , Word "sentence:"
                                                        , Space
                                                        , Reference
                                                            ( Label
                                                                { unLabel = "a_sentence"
                                                                }
                                                            )
                                                        , Word "."
                                                        ]
                                                    )
                                                ]
                                            )
                                        )
                                    )
                                ]
                            )
                        )
                        ( Flagged
                            False
                            [ SimpleSection
                                (SimpleSectionFormat {ssHasPrecedingHorizontalBar = False})
                                []
                            , SimpleSection
                                (SimpleSectionFormat {ssHasPrecedingHorizontalBar = True})
                                [ SimpleParagraph
                                    ( SimpleParagraphFormat
                                        (Typography LeftAligned MediumFontSize [])
                                    )
                                    [ Styled
                                        Bold
                                        [ Word "Artikel"
                                        , Space
                                        , Word "42"
                                        , Space
                                        , Word "der"
                                        , Space
                                        , Word "Änderungssatzung"
                                        , Space
                                        , Word "vom"
                                        , Space
                                        , Word "19."
                                        , Space
                                        , Word "Januar"
                                        , Space
                                        , Word "2038:"
                                        ]
                                    , Space
                                    , Word "Diese"
                                    , Space
                                    , Word "Satzung"
                                    , Space
                                    , Word "tritt"
                                    , Space
                                    , Word "am"
                                    , Space
                                    , Word "Tag"
                                    , Space
                                    , Word "nach"
                                    , Space
                                    , Word "ihrer"
                                    , Space
                                    , Word "Bekanntmachung"
                                    , Space
                                    , Word "in"
                                    , Space
                                    , Word "Kraft."
                                    ]
                                ]
                            ]
                        )
                    )
                    ( fromList
                        [
                            ( Label
                                { unLabel = "a_footnote"
                                }
                            , Footnote
                                SuperscriptFootnoteFormat
                                [ Word "The"
                                , Space
                                , Word "footnote."
                                ]
                            )
                        ]
                    )
                )
            )
            [ Flagged
                False
                ( AppendixSection
                    ( AppendixSectionFormat
                        (AppendixSectionTitle "Anlagen")
                        ( AppendixElementFormat
                            (FormatString [PlaceholderAtom Arabic])
                            ( TocKeyFormat
                                ( FormatString
                                    [ StringAtom "Anlage "
                                    , PlaceholderAtom KeyIdentifierPlaceholder
                                    ]
                                )
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
                    []
                )
            , Flagged
                False
                ( AppendixSection
                    ( AppendixSectionFormat
                        (AppendixSectionTitle "Anhänge")
                        ( AppendixElementFormat
                            (FormatString [PlaceholderAtom Arabic])
                            ( TocKeyFormat
                                ( FormatString
                                    [ StringAtom "Anhang "
                                    , PlaceholderAtom KeyIdentifierPlaceholder
                                    ]
                                )
                            )
                            ( HeadingFormat
                                (Typography LeftAligned LargeFontSize [Bold])
                                ( FormatString
                                    [ StringAtom "Anhang "
                                    , PlaceholderAtom IdentifierPlaceholder
                                    , StringAtom ""
                                    , PlaceholderAtom HeadingTextPlaceholder
                                    ]
                                )
                            )
                        )
                    )
                    [ Flagged
                        False
                        ( Node Nothing $
                            Document
                                (DocumentFormat (Just (TocFormat (TocHeading "Inhaltsverzeichnis"))))
                                ( DocumentHeading
                                    [Word "Beispiel-Überschrift"]
                                )
                                ( DocumentBody
                                    ( Flagged
                                        False
                                        [ SimpleSection
                                            (SimpleSectionFormat {ssHasPrecedingHorizontalBar = False})
                                            [ SimpleParagraph
                                                ( SimpleParagraphFormat
                                                    (Typography Centered LargeFontSize [])
                                                )
                                                [ Word "Vom"
                                                , Space
                                                , Word "19."
                                                , Space
                                                , Word "Januar"
                                                , Space
                                                , Word "2038"
                                                ]
                                            ]
                                        , SimpleSection
                                            (SimpleSectionFormat {ssHasPrecedingHorizontalBar = False})
                                            []
                                        , SimpleSection
                                            (SimpleSectionFormat {ssHasPrecedingHorizontalBar = False})
                                            []
                                        ]
                                    )
                                    ( Flagged
                                        False
                                        ( InnerSectionBody
                                            [ Flagged
                                                True
                                                ( Node
                                                    Nothing
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
                                                            [ Word "section"
                                                            , Space
                                                            , Word "title"
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
                                                                    , Word "is"
                                                                    , Space
                                                                    , Word "a"
                                                                    , Space
                                                                    , Word "simple"
                                                                    , Space
                                                                    , Word "paragraph"
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
                                                                    , Word "paragraph,"
                                                                    , Space
                                                                    , Word "with"
                                                                    , Space
                                                                    , Word "an"
                                                                    , Space
                                                                    , Word "enumeration:"
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
                                                                                Nothing
                                                                                ( EnumItem
                                                                                    [ Word "first"
                                                                                    , Space
                                                                                    , Word "item"
                                                                                    ]
                                                                                )
                                                                            , Node
                                                                                Nothing
                                                                                ( EnumItem
                                                                                    [ Word "second"
                                                                                    , Space
                                                                                    , Word "item"
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
                                                                                                Nothing
                                                                                                ( EnumItem
                                                                                                    [Word "sub-item"]
                                                                                                )
                                                                                            ]
                                                                                        )
                                                                                    ]
                                                                                )
                                                                            , Node
                                                                                Nothing
                                                                                ( EnumItem
                                                                                    [ Word "third"
                                                                                    , Space
                                                                                    , Word "item,"
                                                                                    , Space
                                                                                    , Word "which"
                                                                                    , Space
                                                                                    , Word "spans"
                                                                                    , Space
                                                                                    , Word "several"
                                                                                    , Space
                                                                                    , Word "lines"
                                                                                    ]
                                                                                )
                                                                            ]
                                                                        )
                                                                    ]
                                                                )
                                                            , Node
                                                                ( Just
                                                                    ( Label
                                                                        { unLabel = "some_paragraph"
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
                                                                    , Word "A"
                                                                    , Space
                                                                    , Word "labeled"
                                                                    , Space
                                                                    , Word "paragraph,"
                                                                    , Space
                                                                    , Word "referencing"
                                                                    , Space
                                                                    , Word "itself:"
                                                                    , Space
                                                                    , Reference
                                                                        ( Label
                                                                            { unLabel = "some_paragraph"
                                                                            }
                                                                        )
                                                                    , Word "."
                                                                    , Space
                                                                    , Special (SentenceStart Nothing)
                                                                    , Word "It"
                                                                    , Space
                                                                    , Word "also"
                                                                    , Space
                                                                    , Word "has"
                                                                    , Space
                                                                    , Word "a"
                                                                    , Space
                                                                    , Word "footnote"
                                                                    , FootnoteRef
                                                                        ( FootnoteReference
                                                                            ( Label
                                                                                { unLabel = "a_footnote"
                                                                                }
                                                                            )
                                                                        )
                                                                    , Word "."
                                                                    , Space
                                                                    , Special
                                                                        ( SentenceStart
                                                                            ( Just
                                                                                ( Label
                                                                                    { unLabel = "a_sentence"
                                                                                    }
                                                                                )
                                                                            )
                                                                        )
                                                                    , Word "It"
                                                                    , Space
                                                                    , Word "also"
                                                                    , Space
                                                                    , Word "has"
                                                                    , Space
                                                                    , Word "a"
                                                                    , Space
                                                                    , Word "self-referencing"
                                                                    , Space
                                                                    , Word "sentence:"
                                                                    , Space
                                                                    , Reference
                                                                        ( Label
                                                                            { unLabel = "a_sentence"
                                                                            }
                                                                        )
                                                                    , Word "."
                                                                    ]
                                                                )
                                                            ]
                                                        )
                                                    )
                                                )
                                            ]
                                        )
                                    )
                                    ( Flagged
                                        False
                                        [ SimpleSection
                                            (SimpleSectionFormat {ssHasPrecedingHorizontalBar = False})
                                            []
                                        , SimpleSection
                                            (SimpleSectionFormat {ssHasPrecedingHorizontalBar = True})
                                            [ SimpleParagraph
                                                ( SimpleParagraphFormat
                                                    (Typography LeftAligned MediumFontSize [])
                                                )
                                                [ Styled
                                                    Bold
                                                    [ Word "Artikel"
                                                    , Space
                                                    , Word "42"
                                                    , Space
                                                    , Word "der"
                                                    , Space
                                                    , Word "Änderungssatzung"
                                                    , Space
                                                    , Word "vom"
                                                    , Space
                                                    , Word "19."
                                                    , Space
                                                    , Word "Januar"
                                                    , Space
                                                    , Word "2038:"
                                                    ]
                                                , Space
                                                , Word "Diese"
                                                , Space
                                                , Word "Satzung"
                                                , Space
                                                , Word "tritt"
                                                , Space
                                                , Word "am"
                                                , Space
                                                , Word "Tag"
                                                , Space
                                                , Word "nach"
                                                , Space
                                                , Word "ihrer"
                                                , Space
                                                , Word "Bekanntmachung"
                                                , Space
                                                , Word "in"
                                                , Space
                                                , Word "Kraft."
                                                ]
                                            ]
                                        ]
                                    )
                                )
                                ( fromList
                                    [
                                        ( Label
                                            { unLabel = "a_footnote"
                                            }
                                        , Footnote
                                            SuperscriptFootnoteFormat
                                            [ Word "The"
                                            , Space
                                            , Word "footnote."
                                            ]
                                        )
                                    ]
                                )
                        )
                    ]
                )
            ]
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
