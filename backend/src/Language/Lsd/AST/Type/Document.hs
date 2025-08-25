module Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , DocumentType (..)
    , DocumentBodyType (..)
    )
where

import Language.Lsd.AST.SimpleRegex (Disjunction, Sequence)
import Language.Lsd.AST.Type (NamedType)
import Language.Lsd.AST.Type.Footnote (FootnoteType)
import Language.Lsd.AST.Type.Section (SectionBodyType)
import Language.Lsd.AST.Type.SimpleSection (SimpleSectionType)

newtype DocumentFormat
    = DocumentFormat
    { docHasTableOfContents :: Bool
    }
    deriving (Show)

data DocumentType
    = DocumentType
        DocumentFormat
        DocumentBodyType
        (Disjunction (NamedType FootnoteType))

data DocumentBodyType
    = -- | document body type
      DocumentBodyType
        (Sequence (NamedType SimpleSectionType))
        -- ^ intro
        (Disjunction SectionBodyType)
        -- ^ main
        (Sequence (NamedType SimpleSectionType))
        -- ^ outro
