-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Extra utils for "Data.Map".
module Util.Maps where
import           Prelude hiding (min, max)
import           Control.Arrow (first)
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

import qualified Util.Seq as Seq

import           Data.Function (on)


getM :: (Ord k, Monoid a) => Map k a -> k -> a
getM m k = Map.findWithDefault mempty k m

filterKey :: (k -> Bool) -> Map k a -> Map k a
filterKey f = Map.filterWithKey (\k _ -> f k)

deleteKeys :: Ord k => [k] -> Map k a -> Map k a
deleteKeys keys m = Map.withoutKeys m (Set.fromList keys)

insertList :: Ord k => [(k, v)] -> Map k v -> Map k v
insertList kvs m = List.foldl' (\m (k, v) -> Map.insert k v m) m kvs

-- | Like 'Data.Map.split', except include a matched key in the above map.
split2 :: Ord k => k -> Map k a -> (Map k a, Map k a)
split2 k fm = (pre, post')
    where
    (pre, at, post) = Map.splitLookup k fm
    post' = maybe post (\v -> Map.insert k v post) at

-- | Split the map into the maps below, within, and above the given range.
-- @low@ to @high@ is half-open, as usual.
split3 :: Ord k => k -> k -> Map k a -> (Map k a, Map k a, Map k a)
split3 low high fm = (below, within, way_above)
    where
    (below, above) = split2 low fm
    (within, way_above) = split2 high above

-- | Return the subset of the map that is between a half-open low and high key.
within :: Ord k => k -> k -> Map k a -> Map k a
within low high fm = let (_, m, _) = split3 low high fm in m

-- | Find the closest key.  If two are equidistant, favor the one below.
lookupClosest :: (Ord k, Num k) => k -> Map k v -> Maybe (k, v)
lookupClosest key m = case (Map.lookupLT key m, Map.lookupGE key m) of
    (Just (k0, v0), Just (k1, v1))
        | key - k0 <= k1 - key -> Just (k0, v0)
        | otherwise -> Just (k1, v1)
    (Nothing, Just kv) -> Just kv
    (Just kv, Nothing) -> Just kv
    (Nothing, Nothing) -> Nothing

invert :: Ord a => Map k a -> Map a k
invert = Map.fromList . map (\(x, y) -> (y, x)) . Map.assocs

-- | TODO Would it be more efficient to do 'fromListWith (++)'?
multimap :: Ord k => [(k, a)] -> Map k [a]
multimap = Map.fromAscList . map (\gs -> (fst (head gs), map snd gs))
    . List.groupBy ((==) `on` fst) . Seq.sort_on fst

-- | Like Map.fromList, but only accept the first of duplicate keys, and also
-- return the rejected duplicates.
unique :: Ord a => [(a, b)] -> (Map a b, [(a, b)])
unique assocs = (Map.fromList pairs, concat rest)
    where
    -- List.sort is stable, so only the first keys will make it into the map.
    separate = unzip . map pair . List.groupBy ((==) `on` fst) . Seq.sort_on fst
    pair (x:xs) = (x, xs)
    pair [] = error "[]: List.groupBy violated its postcondition"
    (pairs, rest) = separate assocs

-- | Make a map, but if any keys collide, omit that key and return it along
-- with the multiple values.
unique2 :: Ord k => [(k, v)] -> (Map k v, [(k, [v])])
unique2 = first Map.fromAscList . Either.partitionEithers . map separate
    . Seq.group_fst
    where
    separate (k, [v]) = Left (k, v)
    separate (k, vs) = Right (k, vs)

-- | Given two maps, pair up the elements in @map1@ with a samed-keyed element
-- in @map2@, if there is one.  Elements that are only in @map1@ or @map2@ will
-- not be included in the output.
zipIntersection :: Ord k => Map k v1 -> Map k v2 -> [(k, v1, v2)]
zipIntersection map1 map2 =
    [(k, v1, v2) | (k, v1) <- Map.assocs map1, Just v2 <- [Map.lookup k map2]]
    -- I could implement with 'pairs', but it would be less efficient.

-- | Pair up elements from each map with equal keys.
pairs :: Ord k => Map k v1 -> Map k v2 -> [(k, Seq.Paired v1 v2)]
pairs map1 map2 = Seq.pair_sorted (Map.toAscList map1) (Map.toAscList map2)
{-# INLINE pairs #-}

paired :: Ord k => Map k v1 -> Map k v2 -> Map k (Seq.Paired v1 v2)
paired map1 map2 = Map.fromAscList (pairs map1 map2)
{-# INLINE paired #-}

-- | Like 'Map.union', but also return a map of rejected duplicate keys from
-- the map on the right.
uniqueUnion :: Ord k => Map k a -> Map k a -> (Map k a, Map k a)
    -- ^ (union, rejected)
uniqueUnion fm1 fm2 = (Map.union fm1 fm2, rejected)
    where rejected = Map.intersection fm2 fm1

-- | Like 'Map.unions', but return a map of the rejected keys.  Like
-- Map.unions, the first key in the list wins.  If there are multiple
-- conflicting keys, only the first one will show up in the reject map.
uniqueUnions :: Ord k => [Map k a] -> (Map k a, Map k a)
uniqueUnions = flip List.foldl' (Map.empty, Map.empty) $
    \(collect, rejected) fm ->
        let (collect2, rejected2) = uniqueUnion collect fm
        in (collect2, Map.union rejected rejected2)

-- | The Data.Map Monoid instance is just a plain union, and doesn't mappend
-- the values.
mappend :: (Ord k, Monoid.Monoid a) => Map k a -> Map k a -> Map k a
mappend = Map.unionWith Monoid.mappend

-- | Like @Map.unionsWith Monoid.mappend@, but collects the values together,
-- in case mconcat is more efficient than successive mappends.
mconcat :: (Ord k, Monoid.Monoid a) => [Map k a] -> Map k a
mconcat ms = Map.fromList [(k, Monoid.mconcat (get k)) | k <- ks]
    where
    ks = Set.toList $ Set.unions $ map Map.keysSet ms
    get k = Maybe.mapMaybe (Map.lookup k) ms
