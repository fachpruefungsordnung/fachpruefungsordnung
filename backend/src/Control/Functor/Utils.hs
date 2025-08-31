module Control.Functor.Utils
    ( TraversableF (traverseF, sequenceF)
    )
where

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
