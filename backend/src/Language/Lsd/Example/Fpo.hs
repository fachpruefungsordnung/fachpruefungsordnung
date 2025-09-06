{-# LANGUAGE OverloadedStrings #-}

-- | Definition of the FPO LSD.
--
--   This only exists because we cannot yet parse LSD.
module Language.Lsd.Example.Fpo
    ( fpoT
    , superSectionT
    , sectionT
    , paragraphT
    , footnoteT
    )
where

import Data.Char (toLower)
import Data.Typography
import Data.Void (Void)
import Language.Lsd.AST.Common
import Language.Lsd.AST.Format
import Language.Lsd.AST.SimpleRegex
import Language.Lsd.AST.Type
import Language.Lsd.AST.Type.AppendixSection
import Language.Lsd.AST.Type.Document
import Language.Lsd.AST.Type.DocumentContainer
import Language.Lsd.AST.Type.Enum
import Language.Lsd.AST.Type.Footnote
import Language.Lsd.AST.Type.Paragraph
import Language.Lsd.AST.Type.Section
import Language.Lsd.AST.Type.SimpleBlock
import Language.Lsd.AST.Type.SimpleParagraph
import Language.Lsd.AST.Type.SimpleSection
import Language.Lsd.AST.Type.Table
import Language.Lsd.AST.Type.Text

fpoT :: NamedType DocumentContainerType
fpoT =
    NamedType "fpo-container" "Fachprüfungsordnung" $
        DocumentContainerType
            ( DocumentContainerFormat
                headerFormat
                footerFormat
                mainDocFormat
            )
            (NavTocHeading "Header")
            mainDocT
            (Sequence [appendixT, attachmentT])
  where
    mainDocFormat =
        MainDocumentFormat
            (Fallback $ NavTocHeading "(Dokument-Titel)")
            ( HeadingFormat
                (Typography Centered LargeFontSize [Bold])
                (FormatString [PlaceholderAtom HeadingTextPlaceholder])
            )

    headerFormat =
        HeaderFooterFormat
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

    footerFormat =
        HeaderFooterFormat
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

appendixT :: NamedType AppendixSectionType
appendixT =
    NamedType "appendix" "Anlagen" $
        AppendixSectionType
            ( AppendixSectionFormat
                (AppendixSectionTitle "Anlagen")
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
            (Star $ Disjunction [simpleDocT])

attachmentT :: NamedType AppendixSectionType
attachmentT =
    NamedType "attachments" "Anhänge" $
        AppendixSectionType
            ( AppendixSectionFormat
                (AppendixSectionTitle "Anhänge")
                ( AppendixElementFormat
                    (FormatString [PlaceholderAtom Arabic])
                    ( TocKeyFormat $
                        FormatString
                            [ StringAtom "Anhang "
                            , PlaceholderAtom KeyIdentifierPlaceholder
                            ]
                    )
                    ( HeadingFormat
                        (Typography LeftAligned LargeFontSize [Bold])
                        ( FormatString
                            [ StringAtom "Anhang "
                            , PlaceholderAtom IdentifierPlaceholder
                            , StringAtom "\n"
                            , PlaceholderAtom HeadingTextPlaceholder
                            ]
                        )
                    )
                )
            )
            (Star $ Disjunction [simpleDocT])

mainDocT :: NamedType DocumentType
mainDocT =
    NamedType "fpo-maindoc" "Fachprüfungsordnung (Hauptdokument)" $
        DocumentType
            (Keyword "!")
            (DocumentFormat $ Just $ TocFormat $ TocHeading "Inhaltsübersicht")
            (DocumentHeadingType plainTextT)
            ( DocumentBodyType
                ( Just $
                    docIntroTF $
                        Sequence
                            [ dateSSecT
                            , publLogSSecT
                            , introSSecT
                            ]
                )
                ( Disjunction
                    [ docMainBodyTF $
                        InnerSectionBodyType (Star sectionT)
                    , docMainBodyTF $
                        InnerSectionBodyType (Star superSectionT)
                    ]
                )
                ( Just $
                    docExtroTF $
                        Sequence
                            [ extroSSecT
                            , legalLogSSecT
                            ]
                )
            )
            (Disjunction [footnoteT])

simpleDocT :: NamedType DocumentType
simpleDocT =
    NamedType "simpledoc" "Einfaches Textdokument" $
        DocumentType
            (Keyword "!")
            (DocumentFormat Nothing)
            (DocumentHeadingType plainTextT)
            ( DocumentBodyType
                Nothing
                ( Disjunction
                    [ docMainBodyTF $
                        SimpleLeafSectionBodyType (Star simpleBlockT)
                    ]
                )
                Nothing
            )
            (Disjunction [footnoteT])

docMainBodyTF :: SectionBodyType -> DocumentMainBodyType
docMainBodyTF =
    DocumentMainBodyType
        (NavTocHeading "Hauptteil")

docIntroTF :: Sequence (NamedType SimpleSectionType) -> DocumentIntroType
docIntroTF =
    DocumentIntroType
        (NavTocHeading "Intro")

docExtroTF :: Sequence (NamedType SimpleSectionType) -> DocumentExtroType
docExtroTF =
    DocumentExtroType
        (NavTocHeading "Extro")

dateSSecT :: NamedType SimpleSectionType
dateSSecT =
    NamedType "date" "Datumsabschnitt" $
        SimpleSectionType
            (Keyword "[date]")
            SimpleSectionFormat {ssHasPrecedingHorizontalBar = False}
            (Star (simpleParagraphTF Centered LargeFontSize))

publLogSSecT :: NamedType SimpleSectionType
publLogSSecT =
    NamedType "publ_log" "Veröffentlichungshistorie" $
        SimpleSectionType
            (Keyword "[publ_log]")
            SimpleSectionFormat {ssHasPrecedingHorizontalBar = False}
            (Star (simpleParagraphTF LeftAligned SmallFontSize))

introSSecT :: NamedType SimpleSectionType
introSSecT =
    NamedType "intro" "Einleitung" $
        SimpleSectionType
            (Keyword "[intro]")
            SimpleSectionFormat {ssHasPrecedingHorizontalBar = False}
            (Star simpleParagraphT)

extroSSecT :: NamedType SimpleSectionType
extroSSecT =
    NamedType "extro" "Schluss" $
        SimpleSectionType
            (Keyword "[extro]")
            SimpleSectionFormat {ssHasPrecedingHorizontalBar = False}
            (Star simpleParagraphT)

legalLogSSecT :: NamedType SimpleSectionType
legalLogSSecT =
    NamedType "legal_log" "Rechtliche Historie" $
        SimpleSectionType
            (Keyword "[legal_log]")
            SimpleSectionFormat {ssHasPrecedingHorizontalBar = True}
            (Star simpleParagraphT)

superSectionT :: NamedType SectionType
superSectionT =
    NamedType "supersection" "Abschnitt" $
        SectionType
            (Keyword "=")
            ( HeadingType
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
                plainTextT
            )
            ( SectionFormat
                (FormatString [PlaceholderAtom Arabic])
                ( TocKeyFormat $
                    FormatString
                        [ StringAtom "Abschnitt "
                        , PlaceholderAtom KeyIdentifierPlaceholder
                        ]
                )
            )
            (InnerSectionBodyType (Star sectionT))

sectionT :: NamedType SectionType
sectionT =
    NamedType "section" "Paragraph" $
        SectionType
            (Keyword "§")
            ( HeadingType
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
                plainTextT
            )
            ( SectionFormat
                (FormatString [PlaceholderAtom Arabic])
                ( TocKeyFormat $
                    FormatString
                        [ StringAtom "§ "
                        , PlaceholderAtom KeyIdentifierPlaceholder
                        ]
                )
            )
            (LeafSectionBodyType (Star paragraphT))

paragraphT :: NamedType ParagraphType
paragraphT =
    NamedType "paragraph" "Absatz" $
        ParagraphType
            ( ParagraphFormat
                (FormatString [PlaceholderAtom Arabic])
                ( ParagraphKeyFormat $
                    FormatString
                        [ StringAtom "("
                        , PlaceholderAtom KeyIdentifierPlaceholder
                        , StringAtom ")"
                        ]
                )
            )
            richTextT

simpleBlockT :: NamedType SimpleBlockType
simpleBlockT =
    NamedType "simple_block" "Einfacher Block" $
        SimpleBlockType simpleParagraphT (Disjunction [dummyTableT])

simpleParagraphT :: NamedType SimpleParagraphType
simpleParagraphT = simpleParagraphTF LeftAligned MediumFontSize

simpleParagraphTF
    :: TextAlignment
    -> FontSize
    -> NamedType SimpleParagraphType
simpleParagraphTF alignment fsize =
    NamedType typeName displayName $
        SimpleParagraphType
            (SimpleParagraphFormat $ Typography alignment fsize [])
            simpleTextT
  where
    typeName =
        TypeName $
            "simple_paragraph_"
                ++ map toLower (show alignment)
                ++ "_"
                ++ map toLower (show fsize)
    displayName =
        DisplayTypeName $
            "Einfacher Absatz (" ++ show alignment ++ ", " ++ show fsize ++ ")"

dummyTableT :: NamedType TableType
dummyTableT =
    NamedType "dummy_table" "Dummy Tabelle" $
        TableType (Keyword "[dummy_table]")

plainTextT :: TextType Void
plainTextT = TextType (Disjunction [])

richTextT :: TextType EnumType
richTextT = TextType (Disjunction [regularEnumT, simpleEnumT])

simpleTextT :: TextType EnumType
simpleTextT = TextType (Disjunction [])

footnoteTextT :: TextType Void
footnoteTextT = plainTextT

-- Enum rules:
--  - Max. 4 levels of regular enums ("1. (a) (aa) (aaa)").
--  - Simple enums ("-") may only occur as leafs.
--    - I.e., they may not contain *any* sub-enums (including simple enums).

maxRegularEnumDepth :: Int
maxRegularEnumDepth = 3

regularEnumT :: NamedType EnumType
regularEnumT =
    NamedType "regular_enum" "Nummerierte Aufzählung" $
        EnumType
            (Keyword "#")
            ( EnumFormat $
                EnumItemFormat
                    (FormatString [PlaceholderAtom Arabic])
                    ( EnumItemKeyFormat $
                        FormatString
                            [ PlaceholderAtom KeyIdentifierPlaceholder
                            , StringAtom "."
                            ]
                    )
            )
            (TextType (Disjunction [enumTF 1, simpleEnumT]))
  where
    enumTF :: Int -> NamedType EnumType
    enumTF depth =
        NamedType typeName displayName $
            EnumType
                (Keyword "#")
                ( EnumFormat $
                    EnumItemFormat
                        ( FormatString $
                            replicate depth (PlaceholderAtom AlphabeticLower)
                        )
                        ( EnumItemKeyFormat $
                            FormatString
                                [ PlaceholderAtom KeyIdentifierPlaceholder
                                , StringAtom ")"
                                ]
                        )
                )
                (TextType (Disjunction nextEnumTs))
      where
        typeName = TypeName $ "regular_enum_" ++ show depth
        displayName =
            DisplayTypeName $
                "Nummerierte Aufzählung (Tiefe " ++ show depth ++ ")"

        nextEnumTs =
            if depth < maxRegularEnumDepth
                then [enumTF (depth + 1), simpleEnumT]
                else [simpleEnumT]

simpleEnumT :: NamedType EnumType
simpleEnumT =
    NamedType "simple_enum" "Strichpunkt-Aufzählung" $
        EnumType
            (Keyword "-")
            ( EnumFormat $
                EnumItemFormat
                    (FormatString [PlaceholderAtom Arabic])
                    (EnumItemKeyFormat $ FormatString [StringAtom "-"])
            )
            (TextType (Disjunction []))

footnoteT :: NamedType FootnoteType
footnoteT =
    NamedType "footnote" "Fußnote" $
        FootnoteType (Keyword "^") SuperscriptFootnoteFormat footnoteTextT
