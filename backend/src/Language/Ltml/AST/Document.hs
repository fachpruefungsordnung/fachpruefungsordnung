module Language.Ltml.AST.Document
    ( Document (..)
    , DocumentTitle (..)
    , DocumentBody (..)
    )
where

import Data.Text (Text)
import Language.Lsd.AST.Type.Document (DocumentFormat)
import Language.Ltml.AST.Section (SectionBody)
import Language.Ltml.AST.SimpleSection (SimpleSection)

data Document
    = Document
        DocumentFormat
        DocumentTitle
        DocumentBody
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
