-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Control flow and monadic utilities.
module Util.Control (
    module Util.Control
    , module Data.Bifunctor, module Control.Monad.Extra, module Util.CallStack
) where
import Data.Bifunctor (Bifunctor(bimap, first, second))
import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import Control.Monad.Extra
       (whenJust, whenJustM, mapMaybeM, whenM, unlessM, ifM, notM, orM, andM,
        findM, anyM, allM)

import qualified Data.Monoid as Monoid

import Util.CallStack (errorStack, errorIO)


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

-- | This is 'Foldable.foldMap' specialized to lists.
mconcatMap :: Monoid.Monoid b => (a -> b) -> [a] -> b
mconcatMap f = Monoid.mconcat . map f

-- | This is actually a mconcatMapM.
--
-- A further generalized version would be:
--
-- > foldMapA :: (Applicative f, Traversable t, Monoid m) =>
-- >    (a -> f m) -> t a -> f m
-- > foldMapA f = fmap Foldable.fold . traverse f
concatMapM :: (Monad m, Monoid.Monoid b) => (a -> m b) -> [a] -> m b
concatMapM f = Monad.liftM Monoid.mconcat . mapM f

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
