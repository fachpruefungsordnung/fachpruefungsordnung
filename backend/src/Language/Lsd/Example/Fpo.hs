{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.Example.Fpo
    ( fpoT
    , superSectionT
    , sectionT
    , paragraphT
    , footnoteT
    )
where

import Data.Typography
import Data.Void (Void)
import Language.Lsd.AST.Common
import Language.Lsd.AST.Format
import Language.Lsd.AST.SimpleRegex
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

fpoT :: DocumentContainerType
fpoT =
    DocumentContainerType
        (DocumentContainerFormat headerFormat footerFormat headingFormat)
        mainDocT
        (Sequence [appendixT, attachmentT])
  where
    headingFormat =
        HeadingFormat
            (Typography Centered LargeFontSize [Bold])
            (FormatString [PlaceholderAtom HeadingTextPlaceholder])

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

appendixT :: AppendixSectionType
appendixT =
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

attachmentT :: AppendixSectionType
attachmentT =
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

mainDocT :: DocumentType
mainDocT =
    DocumentType
        DocumentFormat {docHasTableOfContents = True}
        ( DocumentBodyType
            ( Sequence
                [ dateSSecT
                , publLogSSecT
                , introSSecT
                ]
            )
            ( Disjunction
                [ InnerSectionBodyType (Star sectionT)
                , InnerSectionBodyType (Star superSectionT)
                ]
            )
            ( Sequence
                [ extroSSecT
                , legalLogSSecT
                ]
            )
        )
        (Disjunction [footnoteT])

simpleDocT :: DocumentType
simpleDocT =
    DocumentType
        DocumentFormat {docHasTableOfContents = False}
        ( DocumentBodyType
            (Sequence [])
            (Disjunction [SimpleLeafSectionBodyType (Star simpleBlockT)])
            (Sequence [])
        )
        (Disjunction [footnoteT])

dateSSecT :: SimpleSectionType
dateSSecT =
    SimpleSectionType
        (Keyword "[date]")
        SimpleSectionFormat {ssHasPrecedingHorizontalBar = False}
        (Star (simpleParagraphTF Centered LargeFontSize))

publLogSSecT :: SimpleSectionType
publLogSSecT =
    SimpleSectionType
        (Keyword "[publ_log]")
        SimpleSectionFormat {ssHasPrecedingHorizontalBar = False}
        (Star (simpleParagraphTF LeftAligned SmallFontSize))

introSSecT :: SimpleSectionType
introSSecT =
    SimpleSectionType
        (Keyword "[intro]")
        SimpleSectionFormat {ssHasPrecedingHorizontalBar = False}
        (Star simpleParagraphT)

extroSSecT :: SimpleSectionType
extroSSecT =
    SimpleSectionType
        (Keyword "[extro]")
        SimpleSectionFormat {ssHasPrecedingHorizontalBar = False}
        (Star simpleParagraphT)

legalLogSSecT :: SimpleSectionType
legalLogSSecT =
    SimpleSectionType
        (Keyword "[legal_log]")
        SimpleSectionFormat {ssHasPrecedingHorizontalBar = True}
        (Star simpleParagraphT)

superSectionT :: SectionType
superSectionT =
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

sectionT :: SectionType
sectionT =
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

paragraphT :: ParagraphType
paragraphT =
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

simpleBlockT :: SimpleBlockType
simpleBlockT = SimpleBlockType simpleParagraphT (Disjunction [dummyTableT])

simpleParagraphT :: SimpleParagraphType
simpleParagraphT = simpleParagraphTF LeftAligned MediumFontSize

simpleParagraphTF :: TextAlignment -> FontSize -> SimpleParagraphType
simpleParagraphTF alignment fsize =
    SimpleParagraphType
        (SimpleParagraphFormat $ Typography alignment fsize [])
        simpleTextT

dummyTableT :: TableType
dummyTableT = TableType (Keyword "[dummy_table]")

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

regularEnumT :: EnumType
regularEnumT =
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
    enumTF :: Int -> EnumType
    enumTF depth =
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
        nextEnumTs =
            if depth < maxRegularEnumDepth
                then [enumTF (depth + 1), simpleEnumT]
                else [simpleEnumT]

simpleEnumT :: EnumType
simpleEnumT =
    EnumType
        (Keyword "-")
        ( EnumFormat $
            EnumItemFormat
                (FormatString [PlaceholderAtom Arabic])
                (EnumItemKeyFormat $ FormatString [StringAtom "-"])
        )
        (TextType (Disjunction []))

footnoteT :: FootnoteType
footnoteT = FootnoteType (Keyword "^") SuperscriptFootnoteFormat footnoteTextT
