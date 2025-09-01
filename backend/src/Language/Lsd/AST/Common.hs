{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lsd.AST.Common
    ( KindName (..)
    , TypeName (..)
    , FullTypeName
    , DisplayTypeName (..)
    , Keyword (..)
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
