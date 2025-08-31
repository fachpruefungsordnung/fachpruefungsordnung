{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.CollectionState
    ( CollectionState
    , runCollectionState
    , execCollectionState
    , collect
    )
where

import Control.Monad.State (State, runState, state)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Map.Utils (insert')

-- | A State monad that wraps a map that can only be added ("collected") to.
newtype CollectionState k v a = CollectionState (State (Map k v) a)
    deriving (Functor, Applicative, Monad)

runCollectionState :: CollectionState k v a -> (a, Map k v)
runCollectionState (CollectionState m) = runState m Map.empty

execCollectionState :: CollectionState k v () -> Map k v
execCollectionState = snd . runCollectionState

-- | Add an entry to the collection iff the key is not yet in the collection.
--   The argument state transformation, which returns a value, is only run if
--   the value is to be added.
collect :: (Ord k) => k -> CollectionState k v v -> CollectionState k v ()
collect k (CollectionState mv) = CollectionState $ state $ \s ->
    let (s', s'') = maybe (s, s) (,t'') $ insert' k v s
        (v, t'') = runState mv s'
     in ((), s'')
