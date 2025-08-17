module Language.Lsd.AST.SimpleRegex
    ( Star (..)
    , Disjunction (..)
    , Sequence (..)
    )
where

newtype Star a = Star a

newtype Disjunction a = Disjunction [a]

newtype Sequence a = Sequence [a]
