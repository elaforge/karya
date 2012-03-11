-- | Extra utils for "Data.Map".
module Util.Map where
import Prelude hiding (min, max)
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid


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
pairs :: (Ord k) => Map.Map k v1 -> Map.Map k v2 -> [(k, Maybe v1, Maybe v2)]
pairs map0 map1 = pair_sorted (Map.toAscList map0) (Map.toAscList map1)
    where
    pair_sorted xs [] = [(k, Just v, Nothing) | (k, v) <- xs]
    pair_sorted [] ys = [(k, Nothing, Just v) | (k, v) <- ys]
    pair_sorted x@((k0, v0) : xs) y@((k1, v1) : ys)
        | k0 == k1 = (k0, Just v0, Just v1) : pair_sorted xs ys
        | k0 < k1 = (k0, Just v0, Nothing) : pair_sorted xs y
        | otherwise = (k1, Nothing, Just v1) : pair_sorted x ys

-- | Like Map.union, but also return a map of rejected duplicate keys from the
-- map on the right.
unique_union :: (Ord k) =>
    Map.Map k a -> Map.Map k a -> (Map.Map k a, Map.Map k a)
    -- Map.union is left biased, so dups will be thrown out.
unique_union fm0 fm1 = (Map.union fm0 fm1, lost)
    where lost = Map.intersection fm1 fm0

-- | Map.union says it's more efficient with @big `union` small@, so this one
-- flips the args to be more efficient.  It's still left-biased.
union2 :: (Ord k) => Map.Map k v -> Map.Map k v -> Map.Map k v
union2 m1 m2
    | Map.size m1 < Map.size m2 = m2 `right` m1
    | otherwise = m1 `Map.union` m2
    where right = Map.unionWith (\_ a -> a)

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
