module Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , DocumentType (..)
    , DocumentBodyType (..)
    , PreDocumentType (..)
    , PreDocumentBodyType (..)
    )
where

import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.SimpleRegex (Disjunction, Sequence)
import Language.Lsd.AST.Type.Footnote (FootnoteType, PreFootnoteType)
import Language.Lsd.AST.Type.Section (PreSectionBodyType, SectionBodyType)
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
        (Disjunction FootnoteType)

data PreDocumentType
    = PreDocumentType
        DocumentFormat
        PreDocumentBodyType
        (Disjunction PreFootnoteType)

data DocumentBodyType
    = -- | document body type
      DocumentBodyType
        (Sequence SimpleSectionType)
        -- ^ intro
        (Disjunction SectionBodyType)
        -- ^ main
        (Sequence SimpleSectionType)
        -- ^ outro

data PreDocumentBodyType
    = -- | pre document body type
      PreDocumentBodyType
        (Sequence TypeName)
        -- ^ intro
        (Disjunction PreSectionBodyType)
        -- ^ main
        (Sequence TypeName)
        -- ^ outro
