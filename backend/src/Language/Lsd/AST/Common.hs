{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lsd.AST.Common
    ( KindName (..)
    , TypeName (..)
    , DisplayName (..)
    , Keyword (..)
    )
where

import Data.String (IsString)
import Data.Text (Text)

newtype KindName = KindName String
    deriving (Show, IsString, Eq)

-- TODO: Define somehow for each type.
newtype TypeName = TypeName String
    deriving (Show, IsString, Eq, Ord)

newtype DisplayName = DisplayName String
    deriving (Show, IsString)

newtype Keyword = Keyword Text
