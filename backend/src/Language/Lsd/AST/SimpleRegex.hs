{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lsd.AST.SimpleRegex
    ( Star (..)
    , Disjunction (..)
    , Sequence (..)
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)

newtype Star a = Star a
    deriving (Functor)

newtype Disjunction a = Disjunction [a]
    deriving (Functor, Applicative, Show, Generic)

instance (ToJSON a) => ToJSON (Disjunction a)

instance (FromJSON a) => FromJSON (Disjunction a)

instance (ToSchema a) => ToSchema (Disjunction a)

newtype Sequence a = Sequence [a]
    deriving (Functor)
