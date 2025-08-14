module FPO.Util where

import Prelude

import Control.Alternative (guard)
import Data.Array (length, take)

-- | Prepends an element to an array if the condition is true.
prependIf :: forall a. Boolean -> a -> Array a -> Array a
prependIf cond x xs = (guard cond $> x) <> xs

isPrefixOf :: forall a. Eq a => Array a -> Array a -> Boolean
isPrefixOf prefix arr =
  take (length prefix) arr == prefix

isRealPrefixOf :: forall a. Eq a => Array a -> Array a -> Boolean
isRealPrefixOf prefix arr =
  let
    len = length prefix
  in
    len > 0 && isPrefixOf prefix arr && length arr > len
