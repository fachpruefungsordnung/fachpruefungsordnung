module Language.Ltml.AST.Document
    ( Document (..)
    , DocumentTitle (..)
    , DocumentBody (..)
    )
where

import Data.Map (Map)
import Data.Text (Text)
import Language.Lsd.AST.Type.Document (DocumentFormat)
import Language.Ltml.AST.Footnote (Footnote)
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Section (SectionBody)
import Language.Ltml.AST.SimpleSection (SimpleSection)

data Document
    = Document
        DocumentFormat
        DocumentTitle
        DocumentBody
        (Map Label Footnote)
    deriving (Show)

newtype DocumentTitle = DocumentTitle Text
    deriving (Show)

data DocumentBody
    = -- | document body
      DocumentBody
        [SimpleSection]
        -- ^ intro
        SectionBody
        -- ^ main
        [SimpleSection]
        -- ^ outro
    deriving (Show)
