{-# LANGUAGE DeriveFunctor #-}

module Language.Ltml.Common
    ( Flagged (..)
    , Flagged'
    , flagMap
    , ParseError
    , Parsed
    )
where

import Control.Functor.Utils (TraversableF (traverseF))
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

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

type ParseError = ParseErrorBundle Text Void

-- | @'Parsed' x@ denotes a node that may (!) have been parsed from text
--   (a leaf of the input tree), in which case parsing may have failed.
--   It only ever wraps nodes that correspond to nodes in the input tree.
--   I.e., this is used with nodes that can correspond to leaf nodes in the
--   input tree.
type Parsed = Either ParseError
