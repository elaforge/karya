{- | Control flow and monadic utilities.
-}
module Util.Control where

-- | Map a function across a list threading a state through.  If you're too
-- lazy to use a monad...
map_state :: (st -> a -> (st, b)) -> st -> [a] -> [b]
map_state _ _ [] = []
map_state f state (x:xs) = let (state', v) = f state x
    in v : map_state f state' xs

map_state_m :: (Monad m) => st -> (st -> a -> m (st, b)) -> [a] -> m [b]
map_state_m _ _ [] = return []
map_state_m state f (x:xs) = do
    (state', val) <- f state x
    rest <- map_state_m state' f xs
    return (val : rest)

-- This is like Applicate.<*, but doesn't need an Applicative instance (e.g.
-- Parsec doesn't have one).
(#>>) :: Monad m => m a -> m b -> m a
m1 #>> m2 = do
    v <- m1
    m2
    return v

