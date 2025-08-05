module Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , DocumentType (..)
    , PreDocumentType (..)
    )
where

import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.SimpleRegex (SimpleRegex)
import Language.Lsd.AST.Type.Section (SectionType)

data DocumentFormat = DocumentFormat
    deriving (Show)

data DocumentType
    = DocumentType
        DocumentFormat
        (SimpleRegex SectionType)

data PreDocumentType
    = PreDocumentType
        DocumentFormat
        (SimpleRegex TypeName)
