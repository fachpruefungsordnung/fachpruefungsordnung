module Language.Lsd.AST.Type.Section
    ( SectionFormat (..)
    , SectionType (..)
    , PreSectionType (..)
    , HeadingType (..)
    , PreHeadingType (..)
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.Format
    ( HeadingFormat
    , IdentifierFormat
    , TocKeyFormat
    )
import Language.Lsd.AST.SimpleRegex (SimpleRegex)
import Language.Lsd.AST.Type.Paragraph (ParagraphType)
import Language.Lsd.AST.Type.Text (PreTextType, TextType)

data SectionFormat
    = SectionFormat
        IdentifierFormat
        TocKeyFormat
    deriving (Show)

data SectionType
    = -- | Section type
      SectionType
        Keyword
        (Maybe HeadingType)
        SectionFormat
        (Either ParagraphType (SimpleRegex SectionType))
        -- ^ children's type(s)

data PreSectionType
    = PreSectionType
        Keyword
        (Maybe PreHeadingType)
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
