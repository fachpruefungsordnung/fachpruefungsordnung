{-# LANGUAGE DeriveFunctor #-}

module Language.Ltml.AST.Node
    ( Node (..)
    )
where

import Control.Functor.Utils (Pure (pure'))
import Language.Ltml.AST.Label (Label)

data Node a = Node (Maybe Label) a
    deriving (Show, Functor)

instance Pure Node where
    pure' = Node Nothing
