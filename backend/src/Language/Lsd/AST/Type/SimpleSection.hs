module Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionFormat (..)
    , SimpleSectionType (..)
    , PreSimpleSectionType (..)
    )
where

import Language.Lsd.AST.Common (Keyword, TypeName)
import Language.Lsd.AST.SimpleRegex (Star)
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
        (Star SimpleParagraphType)

data PreSimpleSectionType
    = PreSimpleSectionType
        Keyword
        SimpleSectionFormat
        (Star TypeName)
