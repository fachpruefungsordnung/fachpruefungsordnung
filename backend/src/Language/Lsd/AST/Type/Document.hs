{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , DocumentType (..)
    , DocumentHeadingType (..)
    , DocumentBodyType (..)
    , DocumentMainBodyType (..)
    )
where

import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Disjunction, Sequence)
import Language.Lsd.AST.Type
    ( NamedType
    , ProperNodeKind (..)
    , RawProperNodeKind (..)
    )
import Language.Lsd.AST.Type.Footnote (FootnoteType)
import Language.Lsd.AST.Type.Section (SectionBodyType (..))
import Language.Lsd.AST.Type.SimpleSection (SimpleSectionType)
import Language.Lsd.AST.Type.Text (TextType)

newtype DocumentFormat
    = DocumentFormat
    { docHasTableOfContents :: Bool
    }
    deriving (Show)

data DocumentType
    = DocumentType
        Keyword
        DocumentFormat
        DocumentHeadingType
        DocumentBodyType
        (Disjunction (NamedType FootnoteType))

instance RawProperNodeKind DocumentType where
    kindNameOfRaw _ = "document"

newtype DocumentHeadingType = DocumentHeadingType (TextType Void)

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

instance ProperNodeKind DocumentMainBodyType where
    kindNameOf _ = "document-mainbody"

    typeNameOf (DocumentMainBodyType t') = aux t'
      where
        aux (InnerSectionBodyType _) = "inner"
        aux (LeafSectionBodyType _) = "leaf"
        aux (SimpleLeafSectionBodyType _) = "simple-leaf"

    displayTypeNameOf (DocumentMainBodyType t') = aux t'
      where
        aux (InnerSectionBodyType _) = "nested main body"
        aux (LeafSectionBodyType _) = "textual main body"
        aux (SimpleLeafSectionBodyType _) = "simple textual main body"
