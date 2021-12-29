-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- | Control flow and monadic utilities.
module Util.Control (
    module Util.Control
    , module Data.Bifunctor, module Control.Monad.Extra, module Util.CallStack
) where
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import           Control.Monad.Extra
    (allM, andM, anyM, findM, mapMaybeM, notM, orM, partitionM)
import qualified Control.Monad.Fix as Fix

import           Data.Bifunctor (Bifunctor(bimap, first, second))

import           Util.CallStack (errorIO, errorStack)


-- These are the same as Control.Monad.Extra, but they are frequently used, and
-- by defining them here I can explicitly INLINE them.  Surely they're short
-- enough that ghc will inline anyway, but -fprof-auto-exported isn't that
-- clever.  I got around by recompiling all of hackage with
-- 'profiling-detail: none', but I might as well keep the definitions anyway
-- since it gives me more control.

{-# INLINE whenJust #-}
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = maybe (pure ()) f ma

{-# INLINE whenJustM #-}
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mma f = mma >>= \case
    Nothing -> pure ()
    Just a -> f a

{-# INLINE whenM #-}
whenM :: Monad m => m Bool -> m () -> m ()
whenM mb true = ifM mb true (pure ())

{-# INLINE unlessM #-}
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb false = ifM mb (pure ()) false

{-# INLINE ifM #-}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb true false = mb >>= \case
    True -> true
    False -> false

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- * local

while :: Monad m => m Bool -> m a -> m [a]
while cond op = do
    b <- cond
    case b of
        True -> do
            val <- op
            rest <- while cond op
            return (val:rest)
        False -> return []

while_ :: Monad m => m Bool -> m a -> m ()
while_ cond op = do
    b <- cond
    Monad.when b $ op >> while_ cond op

-- | Loop with no arguments.  This is the same as 'Fix.fix' but the name is
-- clearer.
loop0 :: (a -> a) -> a
loop0 = Fix.fix

-- | Loop with a single state argument.
loop1 :: forall state a. state -> ((state -> a) -> state -> a) -> a
loop1 state f = f again state
    where
    again :: state -> a
    again = f again

-- | Loop with two state arguments.  You could use loop1 with a pair, but
-- sometimes the currying is convenient.
loop2 :: forall s1 s2 a. s1 -> s2 -> ((s1 -> s2 -> a) -> s1 -> s2 -> a) -> a
loop2 s1 s2 f = f again s1 s2
    where
    again :: s1 -> s2 -> a
    again = f again

-- | This is 'Foldable.foldMap' specialized to lists.
mconcatMap :: Monoid b => (a -> b) -> [a] -> b
mconcatMap f = mconcat . map f

-- | This is actually a mconcatMapM.
--
-- A further generalized version would be:
--
-- > foldMapA :: (Applicative f, Traversable t, Monoid m) =>
-- >    (a -> f m) -> t a -> f m
-- > foldMapA f = fmap Foldable.fold . traverse f
concatMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
concatMapM f = Monad.liftM mconcat . mapM f

-- | Run the second action only if the first action returns Just.
--
-- This is like MaybeT, but using MaybeT itself required lots of annoying
-- explicit lifting.
justm :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
justm op1 op2 = maybe (return Nothing) op2 =<< op1

-- | The Either equivalent of 'justm'.  EitherT solves the same problem, but
-- requires a runEitherT and lots of hoistEithers.
rightm :: Monad m => m (Either err a) -> (a -> m (Either err b))
    -> m (Either err b)
rightm op1 op2 = op1 >>= \x -> case x of
    Left err -> return (Left err)
    Right val -> op2 val

{-
    I could generalize justm and rightm with:

    bind2 :: (Monad m1, Traversable m2, Monad m2)
        => m1 (m2 a) -> (a -> m1 (m2 b)) -> m1 (m2 b)
    bind2 ma mb = ma >>= traverse mb >>= return . Monad.join

    But I can't think of any other Traversables I want.
-}

-- | Return the first action to return Just.
firstJust :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
firstJust action alternative = maybe alternative (return . Just) =<< action

-- | 'firstJust' applied to a list.
firstJusts :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJusts = foldr firstJust (return Nothing)

-- * errors

-- The names are chosen to be consistent with the @errors@ package.

-- | Throw on Nothing.
justErr :: err -> Maybe a -> Either err a
justErr err = maybe (Left err) Right

-- | I usually call this @require@.
tryJust :: Except.MonadError e m => e -> Maybe a -> m a
tryJust err = maybe (Except.throwError err) return

-- | I usually call this @require_right@.
tryRight :: Except.MonadError e m => Either e a -> m a
tryRight = either Except.throwError return

rethrow :: Except.MonadError e m => (e -> e) -> m a -> m a
rethrow modify action = action `Except.catchError` \e ->
    Except.throwError (modify e)
