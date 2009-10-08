{- | Control flow and monadic utilities.
-}
module Util.Control where
import Control.Monad.Error () -- get instance Monad (Either e)

-- | Like mapAccumL but lifted into a monad.
map_accuml_m :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m [y]
map_accuml_m _ _ [] = return []
map_accuml_m f accum (x:xs) = do
    (accum', val) <- f accum x
    rest <- map_accuml_m f accum' xs
    return (val : rest)

-- This is like Applicative.<*, but doesn't need an Applicative instance (e.g.
-- Parsec doesn't have one).
(#>>) :: Monad m => m a -> m b -> m a
m1 #>> m2 = do
    v <- m1
    m2
    return v

while :: (Monad m) => m Bool -> m a -> m [a]
while cond op = do
    b <- cond
    case b of
        True -> do
            val <- op
            rest <- while cond op
            return (val:rest)
        False -> return []

while_ :: (Monad m) => m Bool -> m a -> m ()
while_ cond op = do
    b <- cond
    if b then op >> while_ cond op else return ()

whenM :: (Monad m) => m Bool -> m a -> m ()
whenM cond op = do
    b <- cond
    if b then op >> return () else return ()
