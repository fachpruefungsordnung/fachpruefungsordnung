{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Lsd.AST.Type.Section
    ( SectionFormat (..)
    , SectionFormatted (..)
    , SectionType (..)
    , FormattedSectionType
    , HeadingType (..)
    , SectionBodyType (..)
    , sectionBodyChildrenOrderMap
    )
where

import Control.Functor.Utils (TraversableF (traverseF))
import Data.Void (Void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Format
    ( IdentifierFormat
    , InnerHeadingFormat
    , TocKeyFormat
    )
import Language.Lsd.AST.SimpleRegex (Star (Star))
import Language.Lsd.AST.Type
    ( ChildrenOrder (StarOrder)
    , HasEditableHeader (HasEditableHeader)
    , NamedType
    , ProperNodeKind
    , RawProperNodeKind (..)
    , TreeSyntax (LeafSyntax, TreeSyntax)
    )
import Language.Lsd.AST.Type.Paragraph (ParagraphType)
import Language.Lsd.AST.Type.SimpleBlock (SimpleBlockType)
import Language.Lsd.AST.Type.Text (TextType)

data SectionFormat
    = SectionFormat
        IdentifierFormat
        TocKeyFormat
    deriving (Show)

data SectionFormatted a = SectionFormatted SectionFormat a
    deriving (Show, Functor)

instance TraversableF SectionFormatted where
    traverseF f (SectionFormatted fmt x) = SectionFormatted fmt <$> f x

type FormattedSectionType = SectionFormatted SectionType

data SectionType
    = SectionType
        Keyword
        HeadingType
        SectionBodyType

instance RawProperNodeKind FormattedSectionType where
    kindNameOfRaw _ = "section"

    -- Note: The parser also permits super-sections as leafs in the input tree.
    --  - Ideally, this should also somehow be reflected here. (TODO)
    treeSyntaxMapRaw f (SectionFormatted _ (SectionType _ _ bodyT)) =
        case sectionBodyChildrenOrderMap f bodyT of
            Just co -> TreeSyntax (HasEditableHeader True) co
            Nothing -> LeafSyntax

data HeadingType
    = HeadingType
        InnerHeadingFormat
        (TextType Void)

data SectionBodyType
    = InnerSectionBodyType (Star (NamedType FormattedSectionType))
    | LeafSectionBodyType (Star (NamedType ParagraphType))
    | SimpleLeafSectionBodyType (Star (NamedType SimpleBlockType))

sectionBodyChildrenOrderMap
    :: (forall t'. (ProperNodeKind t') => t' -> a)
    -> SectionBodyType
    -> Maybe (ChildrenOrder a)
sectionBodyChildrenOrderMap f (InnerSectionBodyType (Star t)) =
    Just $ StarOrder $ pure $ f t
sectionBodyChildrenOrderMap _ _ = Nothing
