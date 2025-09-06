{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , TocFormat (..)
    , TocHeading (..)
    , DocumentType (..)
    , DocumentHeadingType (..)
    , DocumentBodyType (..)
    , DocumentMainBodyType (..)
    , DocumentIntroType (..)
    , DocumentExtroType (..)
    )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword, NavTocHeading)
import Language.Lsd.AST.SimpleRegex (Disjunction, Sequence)
import Language.Lsd.AST.Type
    ( ChildrenOrder (SequenceOrder)
    , HasEditableHeader (HasEditableHeader)
    , NamedType
    , ProperNodeKind (..)
    , RawProperNodeKind (..)
    , TreeSyntax (LeafSyntax, TreeSyntax)
    )
import Language.Lsd.AST.Type.Footnote (FootnoteType)
import Language.Lsd.AST.Type.Section
    ( SectionBodyType (..)
    , sectionBodyChildrenOrderMap
    )
import Language.Lsd.AST.Type.SimpleSection (SimpleSectionType)
import Language.Lsd.AST.Type.Text (TextType)

newtype DocumentFormat
    = -- | ()
      DocumentFormat
        (Maybe TocFormat)
        -- ^ @Just fmt@ iff a TOC is desired.
    deriving (Show)

newtype TocFormat
    = TocFormat
        TocHeading
    deriving (Show)

newtype TocHeading = TocHeading Text
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

    treeSyntaxMapRaw f (DocumentType _ _ _ bodyT _) =
        TreeSyntax (HasEditableHeader True) $ aux bodyT
      where
        aux (DocumentBodyType introT mainT extroT) =
            SequenceOrder . catMaybes $
                [ pure . f <$> introT
                , Just $ fmap f mainT
                , pure . f <$> extroT
                ]

newtype DocumentHeadingType = DocumentHeadingType (TextType Void)

data DocumentBodyType
    = DocumentBodyType
        (Maybe DocumentIntroType)
        (Disjunction DocumentMainBodyType)
        (Maybe DocumentExtroType)

data DocumentMainBodyType
    = DocumentMainBodyType
        NavTocHeading
        SectionBodyType

instance ProperNodeKind DocumentMainBodyType where
    kindNameOf _ = "document-mainbody"

    typeNameOf (DocumentMainBodyType _ t') = aux t'
      where
        aux (InnerSectionBodyType _) = "inner"
        aux (LeafSectionBodyType _) = "leaf"
        aux (SimpleLeafSectionBodyType _) = "simple-leaf"

    displayTypeNameOf (DocumentMainBodyType _ t') = aux t'
      where
        aux (InnerSectionBodyType _) = "nested main body"
        aux (LeafSectionBodyType _) = "textual main body"
        aux (SimpleLeafSectionBodyType _) = "simple textual main body"

    treeSyntaxMap f (DocumentMainBodyType _ t) =
        case sectionBodyChildrenOrderMap f t of
            Just co -> TreeSyntax (HasEditableHeader False) co
            Nothing -> LeafSyntax

data DocumentIntroType
    = DocumentIntroType
        NavTocHeading
        (Sequence (NamedType SimpleSectionType))

instance ProperNodeKind DocumentIntroType where
    kindNameOf _ = "document-intro"
    typeNameOf _ = ""
    displayTypeNameOf _ = "document intro"
    treeSyntaxMap _ _ = LeafSyntax

data DocumentExtroType
    = DocumentExtroType
        NavTocHeading
        (Sequence (NamedType SimpleSectionType))

instance ProperNodeKind DocumentExtroType where
    kindNameOf _ = "document-extro"
    typeNameOf _ = ""
    displayTypeNameOf _ = "document extro"
    treeSyntaxMap _ _ = LeafSyntax
