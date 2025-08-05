module Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (..)
    , DocumentContainerHeader (..)
    )
where

import Language.Lsd.AST.Type.DocumentContainer (DocumentContainerFormat)
import Language.Ltml.AST.AppendixSection (AppendixSection)
import Language.Ltml.AST.Document (Document)

data DocumentContainer
    = DocumentContainer
        DocumentContainerFormat
        DocumentContainerHeader
        Document
        [AppendixSection]
    deriving (Show)

data DocumentContainerHeader = DocumentContainerHeader
    deriving (Show)
