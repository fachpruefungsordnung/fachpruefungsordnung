{-# LANGUAGE DeriveFunctor #-}

module Language.Ltml.Common
    ( Flagged (..)
    , Flagged'
    , flagMap
    )
where

import Control.Functor.Utils (TraversableF (traverseF))

data Flagged flag a = Flagged flag a
    deriving (Functor, Show)

-- | Boolean flagging wrapper for input tree nodes
--   ('Language.Ltml.Tree.FlaggedInputTree') and AST nodes.
--
--   The flag indicates whether output should be generated for the respective
--   flagged node.
--
--   Flags always match between corresponding input tree nodes and AST nodes;
--   in particular, in the AST, the flag is only ever positive for nodes that
--   correspond to a node in the input tree.
type Flagged' = Flagged Bool

flagMap :: (fl -> fl') -> Flagged fl a -> Flagged fl' a
flagMap f (Flagged flag x) = Flagged (f flag) x

instance TraversableF (Flagged flag) where
    traverseF f (Flagged flag x) = Flagged flag <$> f x
