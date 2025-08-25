module Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (..)
    , DocumentContainerHeader (..)
    )
where

import Data.Text (Text)
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

data DocumentContainerHeader
    = DocumentContainerHeader
    { dchPdfTitle :: Text
    , dchHeaderFooterSuperTitle :: Text
    , dchHeaderFooterTitle :: Text
    , dchHeaderFooterDate :: Text
    }
    deriving (Show)
