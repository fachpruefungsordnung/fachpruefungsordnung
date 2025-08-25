module Language.Lsd.AST.Type.Section
    ( SectionFormat (..)
    , SectionType (..)
    , PreSectionType (..)
    , HeadingType (..)
    , PreHeadingType (..)
    , SectionBodyType (..)
    , PreSectionBodyType (..)
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.Format
    ( IdentifierFormat
    , InnerHeadingFormat
    , TocKeyFormat
    )
import Language.Lsd.AST.SimpleRegex (Star)
import Language.Lsd.AST.Type.Paragraph (ParagraphType)
import Language.Lsd.AST.Type.SimpleBlock (SimpleBlockType)
import Language.Lsd.AST.Type.Text (PreTextType, TextType)

data SectionFormat
    = SectionFormat
        IdentifierFormat
        TocKeyFormat
    deriving (Show)

data SectionType
    = SectionType
        Keyword
        HeadingType
        SectionFormat
        SectionBodyType

data PreSectionType
    = PreSectionType
        Keyword
        PreHeadingType
        SectionFormat
        PreSectionBodyType

data HeadingType
    = HeadingType
        InnerHeadingFormat
        (TextType Void)

data PreHeadingType
    = PreHeadingType
        InnerHeadingFormat
        (PreTextType Void)

data SectionBodyType
    = InnerSectionBodyType (Star SectionType)
    | LeafSectionBodyType (Star ParagraphType)
    | SimpleLeafSectionBodyType (Star SimpleBlockType)

newtype PreSectionBodyType = PreSectionBodyType (Star TypeName)
