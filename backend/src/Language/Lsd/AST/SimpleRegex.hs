module Language.Lsd.AST.SimpleRegex
    ( SimpleRegex (..)
    , Star (..)
    , Disjunction (..)
    , Sequence (..)
    )
where

data SimpleRegex a
    = SimpleRegex
        (Sequence a)
        (Disjunction (Star (Disjunction a)))
        (Sequence a)

newtype Star a = Star a

newtype Disjunction a = Disjunction [a]

newtype Sequence a = Sequence [a]
