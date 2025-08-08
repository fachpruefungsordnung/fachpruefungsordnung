module FPO.Util where

import Prelude

import Control.Alternative (guard)

-- | Prepends an element to an array if the condition is true.
prependIf :: forall a. Boolean -> a -> Array a -> Array a
prependIf cond x xs = (guard cond $> x) <> xs