module Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (..)
    , DocumentContainerHeader (..)
    )
where

import Data.Text (Text)
import Language.Lsd.AST.Type.DocumentContainer (DocumentContainerFormat)
import Language.Ltml.AST.AppendixSection (AppendixSection)
import Language.Ltml.AST.Document (Document)
import Language.Ltml.Common (Flagged', NavTocHeaded, Parsed)

-- Note: The NavTocHeaded wrapper arguably should better wrap the entire
--   DocumentContainer rather than its header; but this would mean that the
--   root node would be a `NavTocHeaded DocumentContainer`, which seems
--   annoying.
data DocumentContainer
    = DocumentContainer
        DocumentContainerFormat
        (NavTocHeaded (Parsed DocumentContainerHeader))
        (Flagged' Document)
        [Flagged' AppendixSection]
    deriving (Show)

data DocumentContainerHeader = DocumentContainerHeader
    { dchPdfTitle :: Text
    , dchHeaderFooterSuperTitle :: Text
    , dchHeaderFooterTitle :: Text
    , dchHeaderFooterDate :: Text
    }
    deriving (Show)
