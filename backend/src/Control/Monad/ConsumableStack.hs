{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.ConsumableStack
    ( ConsumableStackT
    , ConsumableStack
    , pop
    , runConsumableStackT
    , runConsumableStack
    , ConsumableStackError (..)
    )
where

import Control.Monad ((>=>))
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Bifunctor (first)
import Data.List (uncons)

-- | A monad transformer with a pop-only stack that is expected to be fully
--   consumed ("popped").
newtype ConsumableStackT s m a = ConsumableStackT ([s] -> m (Maybe (a, [s])))

type ConsumableStack s = ConsumableStackT s Identity

unwrapCST :: ConsumableStackT s m a -> [s] -> m (Maybe (a, [s]))
unwrapCST (ConsumableStackT sx) = sx

instance (Functor m) => Functor (ConsumableStackT s m) where
    fmap f sx = ConsumableStackT $ fmap (fmap (first f)) . unwrapCST sx

-- Note: (Applicative m) seems to be an insufficient constraint; see also the
--   StateT instance.
instance (Monad m) => Applicative (ConsumableStackT s m) where
    pure = lift . pure
    sf <*> sx = sf >>= (<$> sx)

instance (Monad m) => Monad (ConsumableStackT s m) where
    sx >>= k = ConsumableStackT $ unwrapCST sx >=> k'
      where
        k' Nothing = return Nothing
        k' (Just (x, s')) = unwrapCST (k x) s'

instance MonadTrans (ConsumableStackT s) where
    lift mx = ConsumableStackT $ \s -> Just . (,s) <$> mx

data ConsumableStackError
    = ConsumableStackDepletedEarly
    | ConsumableStackNotFullyConsumed

class (Monad m) => MonadConsumableStack s m | m -> s where
    -- | Pop the head off the stack.
    pop :: m s

instance (Monad m) => MonadConsumableStack s (ConsumableStackT s m) where
    pop = ConsumableStackT $ pure . uncons

-- | Run a consumable stack monad transformer.
--   Returns a 'ConsumableStackError' iff the stack was depleted early or not
--   fully consumed.
runConsumableStackT
    :: (Functor m)
    => ConsumableStackT s m a
    -> [s]
    -> m (Either ConsumableStackError a)
runConsumableStackT sx stack = f <$> unwrapCST sx stack
  where
    f Nothing = Left ConsumableStackDepletedEarly
    f (Just (x, [])) = Right x
    f (Just (_, _)) = Left ConsumableStackNotFullyConsumed

runConsumableStack
    :: ConsumableStack s a
    -> [s]
    -> Either ConsumableStackError a
runConsumableStack sx = runIdentity . runConsumableStackT sx
