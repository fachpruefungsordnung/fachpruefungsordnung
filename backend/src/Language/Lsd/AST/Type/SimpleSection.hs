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
    ( KindNameOf (kindNameOf)
    , NamedType
    , TypeNameOf (typeNameOf)
    )
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphType)

newtype SimpleSectionFormat
    = SimpleSectionFormat
    { ssHasPrecedingHorizontalBar :: Bool
    }
    deriving (Show)

data SimpleSectionType
    = SimpleSectionType
        Keyword
        SimpleSectionFormat
        (Star (NamedType SimpleParagraphType))

instance KindNameOf (Sequence (NamedType SimpleSectionType)) where
    kindNameOf _ = "simple-section-sequence"

instance TypeNameOf (Sequence (NamedType SimpleSectionType)) where
    typeNameOf _ = ""
