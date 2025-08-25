module Language.Lsd.AST.Type.Section
    ( SectionFormat (..)
    , SectionType (..)
    , HeadingType (..)
    , SectionBodyType (..)
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Format
    ( IdentifierFormat
    , InnerHeadingFormat
    , TocKeyFormat
    )
import Language.Lsd.AST.SimpleRegex (Star)
import Language.Lsd.AST.Type (NamedType)
import Language.Lsd.AST.Type.Paragraph (ParagraphType)
import Language.Lsd.AST.Type.SimpleBlock (SimpleBlockType)
import Language.Lsd.AST.Type.Text (TextType)

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

data HeadingType
    = HeadingType
        InnerHeadingFormat
        (TextType Void)

data SectionBodyType
    = InnerSectionBodyType (Star (NamedType SectionType))
    | LeafSectionBodyType (Star (NamedType ParagraphType))
    | SimpleLeafSectionBodyType (Star (NamedType SimpleBlockType))
