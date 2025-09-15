module Language.Ltml.AST.Document
    ( Document (..)
    , DocumentHeading (..)
    , DocumentBody (..)
    , DocumentMainBody
    , DocumentIntro
    , DocumentExtro
    )
where

import Data.Map (Map)
import Language.Lsd.AST.Type.Document (DocumentFormat)
import Language.Ltml.AST.Footnote (Footnote)
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Section (SectionBody)
import Language.Ltml.AST.SimpleSection (SimpleSection)
import Language.Ltml.AST.Text (HeadingTextTree)
import Language.Ltml.Common (Flagged', NavTocHeaded, Parsed)

data Document
    = Document
        DocumentFormat
        (Parsed DocumentHeading)
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
    = DocumentBody
        (Maybe (Flagged' (NavTocHeaded (Parsed DocumentIntro))))
        (Flagged' (NavTocHeaded (Parsed DocumentMainBody)))
        (Maybe (Flagged' (NavTocHeaded (Parsed DocumentExtro))))
    deriving (Show)

type DocumentMainBody = SectionBody

type DocumentIntro = [SimpleSection]

type DocumentExtro = [SimpleSection]
