{- | Control flow and monadic utilities.
-}
module Util.Control (
    (<$>), (<*>), (<*), (*>), (<|>)
    , first, second
    , (<>), mempty, mconcat
    , while, while_
    , whenM, unlessM, when_just, if_just, ifM, andM, findM

    -- , finally
    , justm
    , fmap0

    -- * lens
    , Lens, (#)
    -- * pure
    , ($#), (=#), (=%)
    -- * state
    , (#>)
    -- , getl, (=:), (=:%)
    , module Control.Monad
) where
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import Control.Monad
import qualified Data.Monoid as Monoid
import Data.Monoid (mempty, mconcat)
import Util.Functor0 (fmap0)
import Util.Lens


-- | Like the Arrow combinators, but specialized to functions for clearer
-- error messages.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (a -> b) -> (c, a) -> (c, b)
second f (c, a) = (c, f a)

(<>) :: (Monoid.Monoid a) => a -> a -> a
(<>) = Monoid.mappend

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

unlessM :: (Monad m) => m Bool -> m a -> m ()
unlessM cond op = do
    b <- cond
    if b then return () else op >> return ()

when_just :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
when_just val f = maybe (return ()) f val

-- | 'maybe' with arguments juggled around to be convenient in a do block.
if_just :: Maybe a -> (a -> b) -> b -> b
if_just m consequent alternative = maybe alternative consequent m

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond consequent alternative = do
    b <- cond
    if b then consequent else alternative

andM :: (Monad m) => [m Bool] -> m Bool
andM [] = return True
andM (c:cs) = do
    b <- c
    if b then andM cs else return False

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = ifM (f x) (return (Just x)) (findM f xs)

-- -- | Finally a finally for MonadError.
-- finally :: (Error.MonadError e m) => m a -> m () -> m a
-- finally action handler =
--     Error.catchError (action >>= \v -> handler >> return v) $
--         \exc -> handler >> Error.throwError exc

-- | This is sort of like a monad transformer, but the Maybe is on the inside
-- instead of the outside.
--
-- What I really want here is MaybeT, but it requres explicit lifting...
justm :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
justm op1 op2 = maybe (return Nothing) op2 =<< op1
