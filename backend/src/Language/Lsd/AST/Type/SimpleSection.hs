{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionFormat (..)
    , SimpleSectionType (..)
    )
where

import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Sequence, Star)
import Language.Lsd.AST.Type
    ( NamedType
    , ProperNodeKind (..)
    , TreeSyntax (LeafSyntax)
    )
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphType)

newtype SimpleSectionFormat = SimpleSectionFormat
    { ssHasPrecedingHorizontalBar :: Bool
    }
    deriving (Show)

data SimpleSectionType
    = SimpleSectionType
        Keyword
        SimpleSectionFormat
        (Star (NamedType SimpleParagraphType))

instance ProperNodeKind (Sequence (NamedType SimpleSectionType)) where
    kindNameOf _ = "simple-section-sequence"
    typeNameOf _ = ""
    displayTypeNameOf _ = "simple section sequence"
    treeSyntaxMap _ _ = LeafSyntax
    kindHasTocHeading _ = False
