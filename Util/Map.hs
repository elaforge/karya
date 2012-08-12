-- | Extra utils for "Data.Map".
module Util.Map where
import Prelude hiding (min, max)
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid

import qualified Util.Seq as Seq


-- | This is just 'findWithDefault' by a shorter name.
get :: (Ord k) => a -> k -> Map.Map k a -> a
get def k fm = Maybe.fromMaybe def (Map.lookup k fm)

-- | This is like 'Map.insert', but do nothing if the key is already present.
soft_insert :: (Ord k) => k -> v -> Map.Map k v -> Map.Map k v
soft_insert k v fm = Map.union fm (Map.singleton k v)

filter_key :: (Ord k) => (k -> Bool) -> Map.Map k a -> Map.Map k a
filter_key f = Map.filterWithKey (\k _ -> f k)

delete_keys :: (Ord k) => [k] -> Map.Map k a -> Map.Map k a
delete_keys keys fm = Map.difference fm (Map.fromList [(k, ()) | k <- keys])

-- | Like 'Data.Map.split', except include a matched key in the above map.
split2 :: (Ord k) => k -> Map.Map k a -> (Map.Map k a, Map.Map k a)
split2 k fm = (pre, post')
    where
    (pre, at, post) = Map.splitLookup k fm
    post' = maybe post (\v -> Map.insert k v post) at

-- | Split the map into the maps below, within, and above the given range.
-- @low@ to @high@ is half-open, as usual.
split3 :: (Ord k) => k -> k -> Map.Map k a
    -> (Map.Map k a, Map.Map k a, Map.Map k a)
split3 low high fm = (below, within, way_above)
    where
    (below, above) = split2 low fm
    (within, way_above) = split2 high above

-- | Return the subset of the map that is between a half-open low and high key.
within :: (Ord k) => k -> k -> Map.Map k a -> Map.Map k a
within low high fm = let (_, m, _) = split3 low high fm in m

lookup_below :: (Ord k) => k -> Map.Map k a -> Maybe (k, a)
lookup_below k m = case Map.splitLookup k m of
    (_, Just v, _) -> Just (k, v)
    (pre, _, _) -> min pre

invert :: (Ord k, Ord a) => Map.Map k a -> Map.Map a k
invert = Map.fromList . map (\(x, y) -> (y, x)) . Map.assocs

-- | TODO Would it be more efficient to do 'fromListWith (++)'?
multimap :: (Ord k) => [(k, a)] -> Map.Map k [a]
multimap = Map.fromAscList . map (\gs -> (fst (head gs), map snd gs))
    . List.groupBy ((==) `on` fst) . List.sortBy (compare `on` fst)

-- | Like Map.fromList, but only accept the first of duplicate keys, and also
-- return the rejected duplicates.
unique :: (Ord a) => [(a, b)] -> (Map.Map a b, [(a, b)])
unique assocs = (Map.fromList pairs, concat rest)
    where
    -- List.sort is stable, so only the first keys will make it into the map.
    separate = unzip . map pair . List.groupBy ((==) `on` fst)
        . List.sortBy (compare `on` fst)
    pair (x:xs) = (x, xs)
    pair [] = error "[]: List.groupBy violated its postcondition"
    (pairs, rest) = separate assocs

-- | Given two maps, pair up the elements in @map1@ with a samed-keyed element
-- in @map2@, if there is one.  Elements that are only in @map1@ or @map2@ will
-- not be included in the output.
zip_intersection :: (Ord k) => Map.Map k v1 -> Map.Map k v2 -> [(k, v1, v2)]
zip_intersection map1 map2 =
    [(k, v1, v2) | (k, v1) <- Map.assocs map1, Just v2 <- [Map.lookup k map2]]
    -- I could implement with 'pairs', but it would be less efficient.

-- | Pair up elements from each map with equal keys.  @(k, Nothing, Nothing)@
-- will never appear in the output.
pairs :: (Ord k) => Map.Map k v1 -> Map.Map k v2 -> [(k, Seq.Paired v1 v2)]
pairs map1 map2 = Seq.pair_sorted (Map.toAscList map1) (Map.toAscList map2)

-- | Like 'Map.union', but also return a map of rejected duplicate keys from
-- the map on the right.
unique_union :: (Ord k) =>
    Map.Map k a -> Map.Map k a -> (Map.Map k a, Map.Map k a)
    -- ^ (union, rejected)
unique_union fm1 fm2 = (Map.union fm1 fm2, rejected)
    where rejected = Map.intersection fm2 fm1

-- | Like 'Map.unions', but return a map of the rejected keys.  Like
-- Map.unions, the first key in the list wins.  If there are multiple
-- conflicting keys, only the first one will show up in the reject map.
unique_unions :: (Ord k) => [Map.Map k a] -> (Map.Map k a, Map.Map k a)
unique_unions = flip List.foldl' (Map.empty, Map.empty) $
    \(collect, rejected) fm ->
        let (collect2, rejected2) = unique_union collect fm
        in (collect2, Map.union rejected rejected2)

insert_list :: (Ord k) => [(k, v)] -> Map.Map k v -> Map.Map k v
insert_list kvs m = List.foldl' (\m (k, v) -> Map.insert k v m) m kvs

-- | Safe versions of findMin and findMax.
min :: Map.Map k a -> Maybe (k, a)
min fm
    | Map.null fm = Nothing
    | otherwise = Just (Map.findMin fm)

max :: Map.Map k a -> Maybe (k, a)
max fm
    | Map.null fm = Nothing
    | otherwise = Just (Map.findMax fm)

-- | The Data.Map Monoid instance is just a plain union, and doesn't mappend
-- the values.
mappend :: (Ord k, Monoid.Monoid a) => Map.Map k a -> Map.Map k a
    -> Map.Map k a
mappend = Map.unionWith Monoid.mappend
