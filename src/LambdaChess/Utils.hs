module LambdaChess.Utils where

-- | The Blackbird combinator.
(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap

-- | Apply function to second argument of binary function.
(.^) :: (Functor f) => f (b -> c) -> (a -> b) -> f (a -> c)
(.^) = (. flip (.)) . flip fmap

-- | Applicative composition.
(<*<) :: (Applicative f) => f (b -> c) -> f (a -> b) -> f (a -> c)
(<*<) = liftA2 (.)
