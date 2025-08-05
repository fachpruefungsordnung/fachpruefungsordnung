module Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (..)
    , DocumentContainerType (..)
    , PreDocumentContainerType (..)
    )
where

import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.Type.AppendixSection (AppendixSectionType)
import Language.Lsd.AST.Type.Document (DocumentType)

data DocumentContainerFormat = DocumentContainerFormat
    deriving (Show)

data DocumentContainerType
    = DocumentContainerType
        DocumentContainerFormat
        DocumentType
        [AppendixSectionType]

data PreDocumentContainerType
    = PreDocumentContainerType
        DocumentContainerFormat
        TypeName
        [TypeName]
