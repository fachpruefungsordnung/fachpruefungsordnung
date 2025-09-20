module Control.Functor.Utils
    ( Pure (pure')
    , TraversableF (traverseF, sequenceF)
    , traverseEither
    , sequenceEither
    )
where

import Control.Monad.Identity (Identity)

-- | Like 'Applicative', but without @<*>@.
--   For any instance of both 'Applicative' and 'Pure', it should hold
--   @pure' = pure@.
class (Functor f) => Pure f where
    pure' :: a -> f a

instance Pure Identity where
    pure' = pure

instance Pure (Either e) where
    pure' = pure

-- | A variant of 'Data.Traversable.Traversable' with weaker constraints.
--
--   In particular, 'traverseF' and 'sequenceF' weaken the 'Applicative'
--   constraint to a 'Functor' constraint.
--
--   Among the type class constraints, the 'Functor' constraint is obvious,
--   for 'fmap' can be implemented using 'traverseF'.
--   The 'Foldable' constraint, however, which is present for
--   'Data.Traversable.Traversable', is debatable, and was removed for
--   convenience.
--
--   It is debatable whether the class and function names are fitting.
class (Functor t) => TraversableF t where
    {-# MINIMAL traverseF | sequenceF #-}

    -- | Like 'Data.Traversable.traverse', but with weaker constraints.
    traverseF :: (Functor f) => (a -> f b) -> t a -> f (t b)
    traverseF f = sequenceF . fmap f

    -- | Like 'Data.Traversable.sequenceA', but with weaker constraints.
    sequenceF :: (Functor f) => t (f a) -> f (t a)
    sequenceF = traverseF id

-- | Like 'Data.Traversable.traverse' and 'traverseF', for 'Either e', and
--   with constraint in between.
--   This could also defined via a corresponding type class, but we'd only
--   need this one instance.
traverseEither :: (Pure f) => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left e) = Left <$> pure' e
traverseEither f (Right x) = Right <$> f x

-- | Variant of 'Data.Traversable.sequenceA', like 'traverseEither' is a
--   variant of 'Data.Traversable.traverse'.
sequenceEither :: (Pure f) => Either e (f a) -> f (Either e a)
sequenceEither = traverseEither id
