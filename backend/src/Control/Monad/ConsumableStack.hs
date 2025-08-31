{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.ConsumableStack
    ( ConsumableStackT
    , pop
    , runConsumableStackT
    )
where

import Control.Monad.State (StateT (StateT), runStateT)
import Control.Monad.Trans.Class (MonadTrans)
import Data.List (uncons)

-- | A monad transformer with a pop-only stack that is expected to be fully
--   consumed ("popped").
newtype ConsumableStackT s m a = ConsumableStackT (StateT [s] m a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadTrans)

class (MonadFail m) => MonadConsumableStack s m | m -> s where
    -- | Pop the head off the stack.
    pop :: m s

instance (MonadFail m) => MonadConsumableStack s (ConsumableStackT s m) where
    pop = ConsumableStackT $ StateT $ f . uncons
      where
        f Nothing = fail "Stack depleted early"
        f (Just x) = return x

-- | Run a consumable stack monad transformer.
--   Fails if the stack was not fully consumed.
runConsumableStackT :: (MonadFail m) => ConsumableStackT s m a -> [s] -> m a
runConsumableStackT (ConsumableStackT m) stack =
    runStateT m stack >>= f
  where
    f (x, []) = return x
    f _ = fail "Stack not fully consumed"
