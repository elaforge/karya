{- | Control flow and monadic utilities.
-}
module Util.Control (
    (<$>), (<*>), (<*), (*>), (<|>)
    , first, second
    , map_accuml_m
    , while, while_
    , whenM, when_just
) where
import Control.Monad
import qualified Control.Monad.Error as Error
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import qualified Control.Applicative as Applicative
import qualified Text.ParserCombinators.Parsec as P


-- Like the Arrow combinators, but specialized to functions.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)
second :: (a -> b) -> (c, a) -> (c, b)
second f (c, a) = (c, f a)

-- | Like mapAccumL but lifted into a monad.
map_accuml_m :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m [y]
map_accuml_m _ _ [] = return []
map_accuml_m f accum (x:xs) = do
    (accum', val) <- f accum x
    rest <- map_accuml_m f accum' xs
    return (val : rest)

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

when_just :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
when_just val f = maybe (return ()) f val


instance Applicative.Applicative (P.GenParser s a) where
    pure = return
    (<*>) = ap

instance Applicative.Alternative (P.GenParser s a) where
    empty = mzero
    (<|>) = mplus

instance (Error.Error e) => Applicative.Applicative (Either e) where
    pure = return
    (<*>) = ap
