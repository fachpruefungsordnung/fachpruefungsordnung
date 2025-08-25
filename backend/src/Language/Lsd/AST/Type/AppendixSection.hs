module Language.Lsd.AST.Type.AppendixSection
    ( AppendixSectionFormat (..)
    , AppendixSectionTitle (..)
    , AppendixElementFormat (..)
    , AppendixSectionType (..)
    )
where

import Data.Text (Text)
import Language.Lsd.AST.Format
    ( IdentifierFormat
    , InnerHeadingFormat
    , TocKeyFormat
    )
import Language.Lsd.AST.SimpleRegex (Disjunction, Star)
import Language.Lsd.AST.Type (NamedType)
import Language.Lsd.AST.Type.Document (DocumentType)

data AppendixSectionFormat
    = AppendixSectionFormat
        AppendixSectionTitle
        AppendixElementFormat
    deriving (Show)

-- | Title of an appendix section.
--   This is not a heading; it only occurs in the TOC, if any.
newtype AppendixSectionTitle = AppendixSectionTitle Text
    deriving (Show)

data AppendixElementFormat
    = AppendixElementFormat
        IdentifierFormat
        TocKeyFormat
        InnerHeadingFormat
    deriving (Show)

data AppendixSectionType
    = AppendixSectionType
        AppendixSectionFormat
        (Star (Disjunction (NamedType DocumentType)))
