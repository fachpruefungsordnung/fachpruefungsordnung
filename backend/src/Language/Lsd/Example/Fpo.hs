{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.Example.Fpo
    ( fpoT
    , superSectionT
    , sectionT
    , paragraphT
    , footnoteT
    )
where

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
import Language.Lsd.AST.Type.Text

fpoT :: DocumentContainerType
fpoT =
    DocumentContainerType
        DocumentContainerFormat
        mainDocT
        (Sequence [appendixT, attachmentT])

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
                ( FormatString
                    [ StringAtom "Anlage "
                    , PlaceholderAtom IdentifierPlaceholder
                    , StringAtom "\n"
                    , PlaceholderAtom HeadingTextPlaceholder
                    ]
                )
            )
        )
        (Star $ Disjunction []) -- TODO

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
                ( FormatString
                    [ StringAtom "Anhang "
                    , PlaceholderAtom IdentifierPlaceholder
                    , StringAtom "\n"
                    , StringAtom "(nicht Bestandteil der Satzung)"
                    , StringAtom "\n"
                    , PlaceholderAtom HeadingTextPlaceholder
                    ]
                )
            )
        )
        (Star $ Disjunction []) -- TODO

mainDocT :: DocumentType
mainDocT =
    DocumentType
        DocumentFormat
        ( DocumentBodyType
            (Sequence [])
            ( Disjunction
                [ InnerSectionBodyType (Star sectionT)
                , InnerSectionBodyType (Star superSectionT)
                ]
            )
            (Sequence [])
        )
        (Disjunction [footnoteT])

superSectionT :: SectionType
superSectionT =
    SectionType
        (Keyword "=")
        ( HeadingType
            ( FormatString
                [ StringAtom "Abschnitt "
                , PlaceholderAtom IdentifierPlaceholder
                , StringAtom " "
                , PlaceholderAtom HeadingTextPlaceholder
                ]
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
            ( FormatString
                [ StringAtom "§ "
                , PlaceholderAtom IdentifierPlaceholder
                , StringAtom "\n"
                , PlaceholderAtom HeadingTextPlaceholder
                ]
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

plainTextT :: TextType Void
plainTextT = TextType (Disjunction [])

richTextT :: TextType EnumType
richTextT = TextType (Disjunction [regularEnumT, simpleEnumT])

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

-- TODO: Unused.
footnoteT :: FootnoteType
footnoteT = FootnoteType (Keyword "^") SuperscriptFootnoteFormat footnoteTextT
