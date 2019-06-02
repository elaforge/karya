-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | EList is a stream of elements, which may either carry a value, or be
-- some kind of metadata, and functions to process values and ignore the
-- metadata.
module Util.EList where
import           Prelude hiding (concatMap, either, map, zip, zip3)
import qualified Control.Monad.Identity as Identity
import qualified Data.Bifunctor as Bifunctor
import           Data.Bifunctor (first, second)


-- TODO I don't like the Elt and Meta names.
-- TODO implement Derive.LEvent and Derive.Call.Post with this.

data Elt e a = Meta !e | Elt !a
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Bifunctor.Bifunctor Elt where
    bimap f _ (Meta e) = Meta (f e)
    bimap _ g (Elt a) = Elt (g a)

metas :: [Elt e a] -> [e]
metas [] = []
metas (Meta e : as) = e : metas as
metas (Elt _ : as) = metas as

elts :: [Elt e a] -> [a]
elts [] = []
elts (Meta _ : as) = elts as
elts (Elt a : as) = a : elts as

partition :: [Elt e a] -> ([e], [a])
partition [] = ([], [])
partition (Meta e : as) = first (e:) (partition as)
partition (Elt a : as) = second (a:) (partition as)

either :: (e -> b) -> (a -> b) -> Elt e a -> b
either f _ (Meta e) = f e
either _ g (Elt a) = g a

fromEither :: Either e a -> Elt e a
fromEither (Left e) = Meta e
fromEither (Right a) = Elt a

toEither :: Elt e a -> Either e a
toEither (Meta e) = Left e
toEither (Elt a) = Right a

map :: (a -> b) -> [Elt e a] -> [Elt e b]
map = fmap . fmap

mapM :: Applicative m => (a -> m b) -> [Elt e a] -> m [Elt e b]
mapM = traverse . traverse

-- | Like 'traverse', but the function can return Meta.
apply :: Applicative m => (a -> m (Elt e b)) -> Elt e a -> m (Elt e b)
apply _ (Meta e) = pure $ Meta e
apply f (Elt a) = f a

-- | Like 'map', except the function can also return Meta.
mapE :: (a -> Elt e b) -> [Elt e a] -> [Elt e b]
mapE f = Identity.runIdentity . mapEM (pure . f)

mapEM :: Applicative m => (a -> m (Elt e b)) -> [Elt e a] -> m [Elt e b]
mapEM f = go
    where
    go (Meta e : as) = (Meta e :) <$> go as
    go (Elt a : as) = (:) <$> f a <*> go as
    go [] = pure []

concatMapE :: (a -> [Elt e b]) -> [Elt e a] -> [Elt e b]
concatMapE f = Identity.runIdentity . concatMapEM (pure . f)

concatMapEM :: Applicative m => (a -> m [Elt e b]) -> [Elt e a] -> m [Elt e b]
concatMapEM f = go
    where
    go (Meta e : as) = (Meta e :) <$> go as
    go (Elt a : as) = (++) <$> f a <*> go as
    go [] = pure []

mapAccumLE :: (state -> a -> (state, Elt e b)) -> state -> [Elt e a]
    -> (state, [Elt e b])
mapAccumLE f = go
    where
    go state (Meta e : as) = second (Meta e :) (go state as)
    go state (Elt a : as) = second (b:) (go state2 as)
        where (state2, b) = f state a
    go state [] = (state, [])

zip :: [a] -> [Elt e b] -> [Elt e (a, b)]
zip as (Meta b : bs) = Meta b : zip as bs
zip (a:as) (Elt b : bs) = Elt (a, b) : zip as bs
zip _ _ = []

zipPaddedSnd :: [a] -> [Elt e b] -> [Elt e (a, Maybe b)]
zipPaddedSnd as (Meta b : bs) = Meta b : zipPaddedSnd as bs
zipPaddedSnd (a:as) (Elt b : bs) = Elt (a, Just b) : zipPaddedSnd as bs
zipPaddedSnd (a:as) [] = Elt (a, Nothing) : zipPaddedSnd as []
zipPaddedSnd [] _ = []

zip3 :: [a] -> [b] -> [Elt e c] -> [Elt e (a, b, c)]
zip3 as bs (Meta c : cs) = Meta c : zip3 as bs cs
zip3 (a:as) (b:bs) (Elt c : cs) = Elt (a, b, c) : zip3 as bs cs
zip3 _ _ _ = []

zip4 :: [a] -> [b] -> [c] -> [Elt e d] -> [Elt e (a, b, c, d)]
zip4 as bs cs (Meta d : ds) = Meta d : zip4 as bs cs ds
zip4 (a:as) (b:bs) (c:cs) (Elt d : ds) = Elt (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []

zipNexts :: [Elt e a] -> [Elt e (a, [a])]
zipNexts (Meta e : as) = Meta e : zipNexts as
zipNexts (Elt a : as) = Elt (a, elts as) : zipNexts as
zipNexts [] = []
