module Control.Applicative.Utils
    ( (<:>)
    )
where

infixl 4 <:>

(<:>) :: (Applicative f) => f a -> f [a] -> f [a]
p <:> q = (:) <$> p <*> q
