{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.Example.Fpo
    ( fpoT
    , superSectionT
    , sectionT
    , paragraphT
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common
import Language.Lsd.AST.Format
import Language.Lsd.AST.SimpleRegex
import Language.Lsd.AST.Type.Document
import Language.Lsd.AST.Type.Enum
import Language.Lsd.AST.Type.Paragraph
import Language.Lsd.AST.Type.Section
import Language.Lsd.AST.Type.Text

fpoT :: DocumentType
fpoT =
    DocumentType
        DocumentFormat
        []
        ( SimpleRegex
            (Sequence [])
            ( Disjunction
                [ Star $ Disjunction [sectionT]
                , Star $ Disjunction [superSectionT]
                ]
            )
            (Sequence [])
        )

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
        )
        ( SimpleRegex
            (Sequence [])
            ( Disjunction
                [ Star $ Disjunction [SectionChildSectionType sectionT]
                ]
            )
            (Sequence [])
        )

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
        )
        ( SimpleRegex
            (Sequence [])
            ( Disjunction
                [Star $ Disjunction [SectionChildParagraphType paragraphT]]
            )
            (Sequence [])
        )

paragraphT :: ParagraphType
paragraphT =
    ParagraphType
        (ParagraphFormat $ FormatString [PlaceholderAtom Arabic])
        richTextT

plainTextT :: TextType Void
plainTextT = TextType [] [footnoteT]

richTextT :: TextType EnumType
richTextT = TextType [enumT] [footnoteT]

footnoteTextT :: TextType Void
footnoteTextT = plainTextT

enumT :: EnumType
enumT = EnumType (Keyword "#") richTextT

footnoteT :: FootnoteType
footnoteT = FootnoteType (Keyword "^") footnoteTextT
