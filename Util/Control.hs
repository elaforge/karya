{- | Control flow and monadic utilities.
-}
module Util.Control (
    (<$>), (<*>), (<*), (*>), (<|>)
    , first, second
    , (<>), mempty, mconcat
    , while, while_
    , whenM, unlessM, when_just, if_just, ifM, andM, orM, findM
    , concatMapM, mapMaybeM
    , mapMaybe, fromMaybe

    , justm, errorIO
    , fmap0

    -- * lens
    , Lens, (#)
    -- * pure
    , (#$), (#=), (%=)
    -- * state
    , (<#>)
    , module Control.Monad
    , lift, liftIO
    -- * nonempty
    , module Data.List.NonEmpty
) where
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift, liftIO)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe, fromMaybe)
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

orM :: (Monad m) => [m Bool] -> m Bool
orM [] = return False
orM (c:cs) = do
    b <- c
    if b then return True else orM cs

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = ifM (f x) (return (Just x)) (findM f xs)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

mapMaybeM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as = go as
    where
    go [] = return []
    go (a:as) = maybe (go as) (\b -> liftM (b:) (go as)) =<< f a

-- | Run the second action only if the first action returns Just.
--
-- This is like MaybeT, but using MaybeT itself required lots of annoying
-- explicit lifting.
justm :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
justm op1 op2 = maybe (return Nothing) op2 =<< op1

errorIO :: (Trans.MonadIO m) => String -> m a
errorIO = Trans.liftIO . Exception.throwIO . Exception.ErrorCall
