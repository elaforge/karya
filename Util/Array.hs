-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Array utilities.
--
-- IArray is awkward and I don't like it.  @vector@ is nicer but it's a big
-- library so I don't want to depend on it unless I have more than one data
-- structure using arrays.
module Util.Array where
import Prelude hiding (null, length)
import qualified Data.Array.IArray as IArray
import Data.Array.IArray ((!))
import qualified Data.List as List


type Array = IArray.Array Int

empty :: Array a
empty = fromList []

null :: Array a -> Bool
null = (==0) . length

length :: Array a -> Int
length a = high - low + 1
    where (low, high) = IArray.bounds a

fromList :: [a] -> Array a
fromList xs = IArray.listArray (0, List.length xs - 1) xs

-- | Like 'IArray.!', except throw a more informative error, with @msg@
-- prepended.
at :: String -> Array a -> Int -> a
at msg a i = a ! assertInBounds msg i a

assertInBounds :: String -> Int -> Array a -> Int
assertInBounds msg i a
    | inBounds i a = i
    | otherwise = error $ msg ++ ": index " ++ show i
        ++ " out of range " ++ show (IArray.bounds a)

-- | Is the given index within the array's bounds?
inBounds :: Int -> Array a -> Bool
inBounds i a = let (low, high) = IArray.bounds a in low <= i && i <= high

-- | Just the array if the index is in bounds.
check :: Int -> Array a -> Maybe (Array a)
check i a
    | inBounds i a = Just a
    | otherwise = Nothing

-- ** searching

-- | Find the index of the first element >= the given element in the sorted
-- array.
bsearch :: Ord k => Array k -> k -> Int
bsearch = bsearchWith (<=)

bsearchOn :: Ord k => (a -> k) -> Array a -> k -> Int
bsearchOn key a elt = bsearchWith (\elt e1 -> (elt <= key e1)) a elt

bsearchWith :: (k -> a -> Bool) -> Array a -> k -> Int
bsearchWith lte a elt = _doBsearch (lte elt) a low (high+1)
    where (low, high) = IArray.bounds a

_doBsearch :: (a -> Bool) -> Array a -> Int -> Int -> Int
_doBsearch lte a low high
    | low == high = low
    | lte (a!mid) = _doBsearch lte a low mid
    | otherwise = _doBsearch lte a (mid+1) high
    where
    mid = (low + high) `div` 2
