{- | This is really more like Util.Map, except the array stuff.
-}
module Util.Data where
import qualified Data.Array.IArray as IArray
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Function
import qualified Data.Map as Map
import Data.Array.IArray ((!))

-- * Map

get :: (Ord k) => a -> k -> Map.Map k a -> a
get def k fm = Maybe.fromMaybe def (Map.lookup k fm)

delete_keys :: (Ord k) => [k] -> Map.Map k a -> Map.Map k a
delete_keys keys fm = Map.difference fm (Map.fromList [(k, ()) | k <- keys])

-- | Like Map.split, except include a matched key in the above map.
split_map :: (Ord k) => k -> Map.Map k a -> (Map.Map k a, Map.Map k a)
split_map k fm = (pre, post')
    where
    (pre, at, post) = Map.splitLookup k fm
    post' = maybe post (\v -> Map.insert k v post) at

-- | Split the map into the maps below, within, and above the given range.
-- @low@ to @high@ is half-open, as usual.
split3_map :: (Ord k) => k -> k -> Map.Map k a
    -> (Map.Map k a, Map.Map k a, Map.Map k a)
split3_map low high fm = (below, within, way_above)
    where
    (below, above) = split_map low fm
    (within, way_above) = split_map high above

within low high fm = let (_, m, _) = split3_map low high fm in m

invert_map :: (Ord k, Ord a) => Map.Map k a -> Map.Map a [k]
invert_map = multimap . map (\(x, y) -> (y, x)) . Map.assocs

-- Would it be more efficient to do 'fromListWith (++)'?
multimap :: (Ord k, Ord a) => [(k, a)] -> Map.Map k [a]
multimap = Map.fromAscList . map (\gs -> (fst (head gs), map snd gs))
    . List.groupBy ((==) `on` fst) . List.sort

-- | Like Map.fromList, but only accept the first of duplicate keys, and also
-- return the rejected duplicates.
unique_map :: (Ord a) => [(a, b)] -> (Map.Map a b, [(a, b)])
unique_map assocs = (Map.fromList pairs, concat rest)
    where
    -- List.sort is stable, so only the first keys will make it into the map.
    separate = unzip . map (\((k, v):rest) -> ((k, v), rest))
        . List.groupBy ((==) `on` fst) . List.sortBy (compare `on` fst)
    (pairs, rest) = separate assocs

-- | Given two maps, pair up the elements in @map1@ with a samed-keyed element
-- in @map2@, if there is one.  Elements that are only in @map1@ or @map2@ will
-- not be included in the output.
zip_intersection :: (Ord k) => Map.Map k v1 -> Map.Map k v2 -> [(k, v1, v2)]
zip_intersection map1 map2 =
    [(k, v1, v2) | (k, v1) <- Map.assocs map1, Just v2 <- [Map.lookup k map2]]

-- | Like Map.union, but also return a map of rejected duplicate keys from the
-- map on the right.
unique_union :: (Ord k) =>
    Map.Map k a -> Map.Map k a -> (Map.Map k a, Map.Map k a)
    -- Map.union is left biased, so dups will be thrown out.
unique_union fm0 fm1 = (Map.union fm0 fm1, lost)
    where lost = Map.intersection fm1 fm0

-- | Safe versions of findMin and findMax.
find_min :: Map.Map k a -> Maybe (k, a)
find_min fm
    | Map.null fm = Nothing
    | otherwise = Just (Map.findMin fm)

find_max :: Map.Map k a -> Maybe (k, a)
find_max fm
    | Map.null fm = Nothing
    | otherwise = Just (Map.findMax fm)

-- * Array

-- | Like 'IArray.!', except throw a more informative error, with @msg@
-- prepended.
at :: (IArray.IArray a e, IArray.Ix i, Show i) => String -> a i e -> i -> e
at msg a i = a ! (assert_in_bounds msg a i)

assert_in_bounds :: (IArray.IArray a e, IArray.Ix i, Show i) =>
    String -> a i e -> i -> i
assert_in_bounds msg a i
    | in_bounds a i = i
    | otherwise = error $ msg ++ ": index " ++ show i
        ++ " out of range " ++ show (IArray.bounds a)

-- | Is the given index within the array's bounds?
in_bounds :: (IArray.IArray a e, IArray.Ix i) => a i e -> i -> Bool
in_bounds a i = let (low, high) = IArray.bounds a in low <= i && i <= high

-- ** searching

-- | Find the index of the first element >= the given element in the sorted
-- array.
bsearch a elt = bsearch_with (<=) a elt

bsearch_on key a elt = bsearch_with (\elt e1 -> (elt <= key e1)) a elt

bsearch_with :: (IArray.IArray a e, IArray.Ix i, Integral i) =>
    (t -> e -> Bool) -> a i e -> t -> i
bsearch_with lte a elt = _do_bsearch (lte elt) a low (high+1)
    where (low, high) = IArray.bounds a

_do_bsearch lte a low high
    | low == high = low
    | lte (a!mid) = _do_bsearch lte a low mid
    | otherwise = _do_bsearch lte a (mid+1) high
    where
    mid = (low + high) `div` 2


t1 :: IArray.Array Int Int
t1 = IArray.listArray (0, 9) [0,2..20]
t2 :: IArray.Array Int Int
t2 = IArray.listArray (0, 5) [0, 0, 1, 1, 2, 2]
t3 :: IArray.Array Int (Int, Char)
t3 = IArray.listArray (0, 5) [(i, 'z') | i <- [0..5]]

u1 = bsearch t1
u2 = bsearch t2
u3 = bsearch_on fst t3
