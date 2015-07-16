-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Control flow and monadic utilities.
module Util.Control where
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans

import qualified Data.Monoid as Monoid


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
    when b $ op >> while_ cond op

whenM :: Monad m => m Bool -> m a -> m ()
whenM cond op = do
    b <- cond
    when b $ op >> return ()

unlessM :: Monad m => m Bool -> m a -> m ()
unlessM cond op = do
    b <- cond
    if b then return () else op >> return ()

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust val f = maybe (return ()) f val

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mval f = mval >>= \val -> whenJust val f

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond consequent alternative = do
    b <- cond
    if b then consequent else alternative

andM :: Monad m => [m Bool] -> m Bool
andM [] = return True
andM (c:cs) = do
    b <- c
    if b then andM cs else return False

orM :: Monad m => [m Bool] -> m Bool
orM [] = return False
orM (c:cs) = do
    b <- c
    if b then return True else orM cs

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = ifM (f x) (return (Just x)) (findM f xs)

-- | This is 'Foldable.foldMap' specialized to lists.
mconcatMap :: Monoid.Monoid b => (a -> b) -> [a] -> b
mconcatMap f = Monoid.mconcat . map f

-- | Or @foldMapA f = fmap Foldable.fold . traverse f@.
concatMapM :: (Monad m, Monoid.Monoid b) => (a -> m b) -> [a] -> m b
concatMapM f = liftM Monoid.mconcat . mapM f

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f as = go as
    where
    go [] = return []
    go (a:as) = maybe (go as) (\b -> liftM (b:) (go as)) =<< f a

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

errorIO :: Trans.MonadIO m => String -> m a
errorIO = Trans.liftIO . Exception.throwIO . Exception.ErrorCall
