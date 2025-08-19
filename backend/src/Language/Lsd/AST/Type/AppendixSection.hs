module Language.Lsd.AST.Type.AppendixSection
    ( AppendixSectionFormat (..)
    , AppendixSectionTitle (..)
    , AppendixElementFormat (..)
    , AppendixSectionType (..)
    , PreAppendixSectionType (..)
    )
where

import Data.Text (Text)
import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.Format
    ( HeadingFormat
    , IdentifierFormat
    , TocKeyFormat
    )
import Language.Lsd.AST.SimpleRegex (Disjunction, Star)
import Language.Lsd.AST.Type.Document (DocumentType)

data AppendixSectionFormat
    = AppendixSectionFormat
        AppendixSectionTitle
        AppendixElementFormat
    deriving (Show)

newtype AppendixSectionTitle = AppendixSectionTitle Text
    deriving (Show)

data AppendixElementFormat
    = AppendixElementFormat
        IdentifierFormat
        TocKeyFormat
        HeadingFormat
    deriving (Show)

data AppendixSectionType
    = AppendixSectionType
        AppendixSectionFormat
        (Star (Disjunction DocumentType))

data PreAppendixSectionType
    = PreAppendixSectionType
        AppendixSectionFormat
        (Star (Disjunction TypeName))
