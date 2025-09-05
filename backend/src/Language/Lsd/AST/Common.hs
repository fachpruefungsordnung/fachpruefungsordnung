{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lsd.AST.Common
    ( KindName (..)
    , TypeName (..)
    , FullTypeName
    , DisplayTypeName (..)
    , Keyword (..)
    , NavTocHeading (..)
    , Fallback (..)
    )
where

import Data.String (IsString)
import Data.Text (Text)

newtype KindName = KindName String
    deriving (Show, IsString, Eq, Ord)

newtype TypeName = TypeName String
    deriving (Show, IsString, Eq, Ord)

type FullTypeName = (KindName, TypeName)

newtype DisplayTypeName = DisplayTypeName String
    deriving (Show, IsString)

newtype Keyword = Keyword Text

-- | Heading for the navigation TOC in the frontend, if static (determined by
--   type only).
newtype NavTocHeading = NavTocHeading Text
    deriving (Show)

-- | A wrapper to denote values that should only be used as fallback;
--   typically in case parsing fails.
newtype Fallback a = Fallback a
    deriving (Show)
