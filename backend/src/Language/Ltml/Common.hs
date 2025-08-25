{-# LANGUAGE DeriveFunctor #-}

module Language.Ltml.Common
    ( Flagged (..)
    )
where

-- | Flagging wrapper for AST nodes.
--   A positive flag indicates that the respective node was originally
--   requested for update.
--   The flag is only ever set for nodes that correspond to a node in the raw
--   tree ('Language.Ltml.Tree.Tree'), thus not during parsing text.
--   The flag should only be set for one node in a tree.
data Flagged a = Flagged Bool a
    deriving (Functor, Show)
