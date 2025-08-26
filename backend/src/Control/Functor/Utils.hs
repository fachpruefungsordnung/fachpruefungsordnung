module Control.Functor.Utils
    ( SequenceF (sequenceF)
    )
where

class SequenceF t where
    -- | Like 'Data.Traversable.sequenceA', but with weaker constraints.
    sequenceF :: (Functor f) => t (f a) -> f (t a)
