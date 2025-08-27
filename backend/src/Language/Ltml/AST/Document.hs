module Language.Ltml.AST.Document
    ( Document (..)
    , DocumentHeading (..)
    , DocumentBody (..)
    )
where

import Data.Map (Map)
import Language.Lsd.AST.Type.Document (DocumentFormat)
import Language.Ltml.AST.Footnote (Footnote)
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Section (SectionBody)
import Language.Ltml.AST.SimpleSection (SimpleSection)
import Language.Ltml.AST.Text (HeadingTextTree)
import Language.Ltml.Common (Flagged)

data Document
    = Document
        DocumentFormat
        DocumentHeading
        DocumentBody
        (Map Label Footnote)
    deriving (Show)

-- | Document heading.
--   Unlike 'Language.Ltml.AST.Section.Heading', this does not incorporate
--   the heading's format, which is instead configured by the
--   'Language.Lsd.AST.Type.DocumentContainerFormat' (for the main document)
--   or 'Language.Lsd.AST.Type.AppendixSectionFormat' (for appendix documents).
newtype DocumentHeading = DocumentHeading [HeadingTextTree]
    deriving (Show)

data DocumentBody
    = -- | document body
      DocumentBody
        (Flagged [SimpleSection])
        -- ^ intro
        (Flagged SectionBody)
        -- ^ main
        (Flagged [SimpleSection])
        -- ^ outro
    deriving (Show)
