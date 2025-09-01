{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lsd.AST.SimpleRegex
    ( Star (..)
    , Disjunction (..)
    , Sequence (..)
    )
where

newtype Star a = Star a
    deriving (Functor)

newtype Disjunction a = Disjunction [a]
    deriving (Functor, Applicative, Show)

newtype Sequence a = Sequence [a]
    deriving (Functor)
