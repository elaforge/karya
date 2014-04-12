-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Control flow and monadic utilities.
-}
module Util.Control (
    Proxy(..)
    , (<$>), (<*>), (<*), (*>), (<|>)
    , first, second, (***)
    , (<>), mempty, mconcat
    , while, while_
    , whenM, unlessM, whenJust, whenJustM, ifM, andM, orM, findM
    , mconcatMap, concatMapM, mapMaybeM
    , mapMaybe, fromMaybe

    , justm, rightm, errorIO
    -- * pretty
    , pretty, prettyt

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
    -- * text
    , Text.Text
    , txt, untxt, showt
) where
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift, liftIO)

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Monoid as Monoid
import Data.Monoid (mempty, mconcat, (<>))
import qualified Data.Text as Text

import Util.Lens
import Util.Pretty (pretty, prettyt)


-- | A value proxy for a type, used for class methods that just want a type,
-- not a value.
data Proxy a = Proxy

-- | Like the Arrow combinators, but specialized to functions for clearer
-- error messages.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (a -> b) -> (c, a) -> (c, b)
second f (c, a) = (c, f a)

(***) :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
f *** g = \(x, y) -> (f x, g y)

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

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust val f = maybe (return ()) f val

whenJustM :: (Monad m) => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mval f = mval >>= \val -> whenJust val f

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

mconcatMap :: Monoid.Monoid b => (a -> b) -> [a] -> b
mconcatMap f = mconcat . Prelude.map f

concatMapM :: (Monad m, Monoid.Monoid b) => (a -> m b) -> [a] -> m b
concatMapM f = liftM mconcat . mapM f

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

-- | The Either equivalent of 'justm'.  EitherT solves the same problem, but
-- requires a runEitherT and lots of hoistEithers.
rightm :: Monad m => m (Either err a) -> (a -> m (Either err b))
    -> m (Either err b)
rightm op1 op2 = op1 >>= \x -> case x of
    Left err -> return (Left err)
    Right val -> op2 val

errorIO :: (Trans.MonadIO m) => String -> m a
errorIO = Trans.liftIO . Exception.throwIO . Exception.ErrorCall

-- * conversion

-- | Utilities to make it easier to convert things to Text.  These are
-- intentionally missing the e to make it easier to search for them.
txt :: String -> Text.Text
txt = Text.pack

untxt :: Text.Text -> String
untxt = Text.unpack

showt :: (Show a) => a -> Text.Text
showt = txt . show
