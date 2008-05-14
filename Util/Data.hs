module Util.Data where

import qualified Data.Array.IArray as IArray
import qualified Data.List as List
import Data.Function
import qualified Data.Map as Map
import Data.Array.IArray ((!))

-- * Map

-- | Like Map.split, except include a matched key in the above map.
split_map :: (Ord k) => k -> Map.Map k a -> (Map.Map k a, Map.Map k a)
split_map k fm = (pre, post')
    where
    (pre, at, post) = Map.splitLookup k fm
    post' = maybe post (\v -> Map.insert k v post) at

invert_map :: (Ord k, Ord a) => Map.Map k a -> Map.Map a [k]
invert_map = multimap . map (\(x, y) -> (y, x)) . Map.assocs

multimap :: (Ord k, Ord a) => [(k, a)] -> Map.Map k [a]
multimap = Map.fromAscList . map (\gs -> (fst (head gs), map snd gs))
    . List.groupBy ((==) `on` fst) . List.sort

-- * Array

-- | Like 'IArray.!', except throw a more informative error, with @msg@
-- prepended.
at :: (IArray.IArray a e, IArray.Ix i, Show i) => String -> a i e -> i -> e
at msg a i
    | i >= low && i <= high = a!i
    | otherwise = error $
        msg ++ ": index " ++ show i ++ " out of range " ++ show (low, high)
    where (low, high) = IArray.bounds a

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
