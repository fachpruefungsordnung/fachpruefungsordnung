{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Lsd.AST.Type.Section
    ( SectionFormat (..)
    , SectionType (..)
    , HeadingType (..)
    , SectionBodyType (..)
    , sectionBodyChildrenOrderMap
    )
where

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
    , NavHeadingGeneration (NavHeadingFromHtmlToc)
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

data SectionType
    = SectionType
        Keyword
        HeadingType
        SectionFormat
        SectionBodyType

instance RawProperNodeKind SectionType where
    kindNameOfRaw _ = "section"

    -- Note: The parser also permits super-sections as leafs in the input tree.
    --  - Ideally, this should also somehow be reflected here. (TODO)
    treeSyntaxMapRaw f (SectionType _ _ _ bodyT) =
        case sectionBodyChildrenOrderMap f bodyT of
            Just co -> TreeSyntax (HasEditableHeader True) co
            Nothing -> LeafSyntax

    navHeadingGenerationOfRaw _ = NavHeadingFromHtmlToc

data HeadingType
    = HeadingType
        InnerHeadingFormat
        (TextType Void)

data SectionBodyType
    = InnerSectionBodyType (Star (NamedType SectionType))
    | LeafSectionBodyType (Star (NamedType ParagraphType))
    | SimpleLeafSectionBodyType (Star (NamedType SimpleBlockType))

sectionBodyChildrenOrderMap
    :: (forall t'. (ProperNodeKind t') => t' -> a)
    -> SectionBodyType
    -> Maybe (ChildrenOrder a)
sectionBodyChildrenOrderMap f (InnerSectionBodyType (Star t)) =
    Just $ StarOrder $ pure $ f t
sectionBodyChildrenOrderMap _ _ = Nothing
