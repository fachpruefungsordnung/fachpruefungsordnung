{-# LANGUAGE FlexibleInstances #-}

module Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionFormat (..)
    , SimpleSectionType (..)
    )
where

import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Star)
import Language.Lsd.AST.Type (NamedType)
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
