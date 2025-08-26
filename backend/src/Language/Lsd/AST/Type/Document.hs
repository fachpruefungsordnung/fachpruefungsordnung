{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , DocumentType (..)
    , DocumentBodyType (..)
    , DocumentMainBodyType (..)
    )
where

import Language.Lsd.AST.SimpleRegex (Disjunction, Sequence)
import Language.Lsd.AST.Type
    ( KindNameOf (kindNameOf)
    , NamedType
    , TypeNameOf (typeNameOf)
    )
import Language.Lsd.AST.Type.Footnote (FootnoteType)
import Language.Lsd.AST.Type.Section (SectionBodyType (..))
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

instance KindNameOf DocumentType where
    kindNameOf _ = "document"

data DocumentBodyType
    = -- | document body type
      DocumentBodyType
        (Sequence (NamedType SimpleSectionType))
        -- ^ intro
        (Disjunction DocumentMainBodyType)
        -- ^ main
        (Sequence (NamedType SimpleSectionType))
        -- ^ outro

newtype DocumentMainBodyType
    = DocumentMainBodyType SectionBodyType

instance KindNameOf DocumentMainBodyType where
    kindNameOf _ = "document-mainbody"

instance TypeNameOf DocumentMainBodyType where
    typeNameOf (DocumentMainBodyType (InnerSectionBodyType _)) = "inner"
    typeNameOf (DocumentMainBodyType (LeafSectionBodyType _)) = "leaf"
    typeNameOf (DocumentMainBodyType (SimpleLeafSectionBodyType _)) =
        "simple-leaf"
