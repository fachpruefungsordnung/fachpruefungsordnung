module Language.Lsd.AST.Type.AppendixSection
    ( AppendixSectionFormat (..)
    , AppendixSectionType (..)
    , PreAppendixSectionType (..)
    )
where

import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.Format (HeadingFormat, IdentifierFormat)
import Language.Lsd.AST.Type.Document (DocumentType)

data AppendixSectionFormat
    = AppendixSectionFormat
        IdentifierFormat
        HeadingFormat
    deriving (Show)

data AppendixSectionType
    = AppendixSectionType
        AppendixSectionFormat
        [DocumentType]

data PreAppendixSectionType
    = PreAppendixSectionType
        AppendixSectionFormat
        [TypeName]
