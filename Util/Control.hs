-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ImplicitParams, ConstraintKinds #-}
-- | Control flow and monadic utilities.
module Util.Control (module Util.Control, module Control.Monad.Extra) where
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Control.Monad.Extra
       (whenJust, whenJustM, mapMaybeM, whenM, unlessM, ifM, notM, orM, andM,
        findM, anyM, allM)
import qualified Control.Monad.Trans as Trans

import qualified Data.Monoid as Monoid
import Data.Monoid ((<>))

import qualified GHC.SrcLoc as SrcLoc
import qualified GHC.Stack as Stack


-- | This is like the hackage bifunctor package, but with no extra
-- dependencies, and no clowns.
class Bifunctor p where
    (***) :: (a -> b) -> (c -> d) -> p a c -> p b d
    first :: (a -> b) -> p a c -> p b c
    second :: (c -> d) -> p a c -> p a d
infixr 3 ***

instance Bifunctor (,) where
    f *** g = \(x, y) -> (f x, g y)
    first f (a, c) = (f a, c)
    second f (c, a) = (c, f a)

instance Bifunctor Either where
    f *** g = either (Left . f) (Right . g)
    first f = either (Left . f) Right
    second f = either Left (Right . f)

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

-- | Return the first action to return Just.
firstJust :: Monad m => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
firstJust action alternative = maybe alternative (return . Just) =<< action

-- | 'firstJust' applied to a list.
firstJusts :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJusts = foldr firstJust (return Nothing)

type Stack = (?stack :: Stack.CallStack)

-- | Just like 'error', except show the caller's location.
errorStack :: Stack => String -> a
errorStack msg = error $ showStack ?stack <> ": " <> msg

-- | Like 'errorStack', except run in IO.
errorIO :: Stack => Trans.MonadIO m => String -> m a
errorIO = Trans.liftIO . Exception.throwIO . Exception.ErrorCall
    . ((showStack ?stack <> ": ") <>)

showStack :: Stack.CallStack -> String
showStack stack = case reverse $ Stack.getCallStack stack of
    (_, srcloc) : _ ->
        SrcLoc.srcLocFile srcloc <> ":" <> show (SrcLoc.srcLocStartLine srcloc)
    [] -> "<no-stack>"
