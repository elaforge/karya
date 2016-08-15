-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
-- | Vector utilities.
module Util.Vector where
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector as V


count :: Generic.Vector v a => (a -> Bool) -> v a -> Int
count f = Generic.foldl' (\c a -> if f a then succ c else c) 0

to_reverse_list :: Generic.Vector v a => v a -> [a]
to_reverse_list vec = map (Generic.unsafeIndex vec) [from, from-1 .. 0]
    where from = Generic.length vec - 1

fold_abort :: Generic.Vector v a => (accum -> a -> Maybe accum) -> accum
    -> v a -> accum
fold_abort f accum vec = go 0 accum
    where go i accum = maybe accum (go (i+1)) $ f accum =<< vec Generic.!? i

-- | Find the index of the last value whose running sum is still below the
-- given number.
find_before :: Generic.Vector v Int => Int -> v Int -> Int
find_before n = fst . fold_abort go (0, 0)
    where
    go (i, total) a
        | total + a <= n = Just (i+1, total+a)
        | otherwise = Nothing

-- | Find the first numbers bracketing @a@.
bracket :: Unboxed.Vector Double -> Double -> Maybe (Int, Double, Double)
    -- ^ (i, low, high) where low <= a < high, and i is the index of low.
    -- If @a@ is out of range, then low==high.
bracket vec a = case Generic.findIndex (>=a) vec of
    Just i
        | get i == a -> Just (i, a, a)
        | i > 0 -> Just (i-1, get (i-1), get i)
    _ -> Nothing
    where get = Generic.unsafeIndex vec

-- | Binary search for the lowest index of the given value, or where it would
-- be if it were present.
{-# SPECIALIZE lowest_index ::
    Ord key => (a -> key) -> key -> V.Vector a -> Int #-}
{-# SPECIALIZE lowest_index ::
    (Generic.Vector Unboxed.Vector a, Ord key) => (a -> key) -> key
        -> Unboxed.Vector a -> Int #-}
{-# INLINEABLE lowest_index #-}
lowest_index :: (Ord key, Generic.Vector v a) => (a -> key) -> key -> v a -> Int
lowest_index key x vec = go vec 0 (Generic.length vec)
    where
    go vec low high
        | low == high = low
        | x <= key (Generic.unsafeIndex vec mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

-- | Binary search for the highest index of the given X.  So the next value is
-- guaranteed to have a higher x, if it exists.  Return -1 if @x@ is before
-- the first element.
{-# SPECIALIZE highest_index ::
    (Generic.Vector Unboxed.Vector a, Ord key) => (a -> key) -> key
        -> Unboxed.Vector a -> Int #-}
{-# INLINEABLE highest_index #-}
highest_index :: (Ord key, Generic.Vector v a) => (a -> key) -> key -> v a
    -> Int
highest_index key x vec
    | Generic.null vec = -1
    | otherwise = i - 1
    where
    i = go 0 (Generic.length vec)
    go low high
        | low == high = low
        | x >= key (Generic.unsafeIndex vec mid) = go (mid+1) high
        | otherwise = go low mid
        where mid = (low + high) `div` 2
