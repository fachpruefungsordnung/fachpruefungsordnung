module Language.Lsd.AST.Type.Section
    ( SectionFormat (..)
    , SectionType (..)
    , PreSectionType (..)
    , HeadingType (..)
    , PreHeadingType (..)
    , SectionChildType (..)
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.Format (HeadingFormat, IdentifierFormat)
import Language.Lsd.AST.SimpleRegex (SimpleRegex)
import Language.Lsd.AST.Type.Paragraph (ParagraphType)
import Language.Lsd.AST.Type.Text (PreTextType, TextType)

newtype SectionFormat
    = SectionFormat
        IdentifierFormat
    deriving (Show)

data SectionType
    = SectionType
        Keyword
        HeadingType
        SectionFormat
        (SimpleRegex SectionChildType)

data PreSectionType
    = PreSectionType
        Keyword
        PreHeadingType
        SectionFormat
        (SimpleRegex TypeName)

data HeadingType
    = HeadingType
        HeadingFormat
        (TextType Void)

data PreHeadingType
    = PreHeadingType
        HeadingFormat
        (PreTextType Void)

data SectionChildType
    = SectionChildSectionType SectionType
    | SectionChildParagraphType ParagraphType
