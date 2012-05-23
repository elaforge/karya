{-# LANGUAGE FlexibleContexts #-}
-- | Vector utilities.
module Util.Vector where
import qualified Data.Vector.Generic as Vector


count :: (Vector.Vector v a) => (a -> Bool) -> v a -> Int
count f = Vector.foldl' (\c a -> if f a then succ c else c) 0

fold_abort :: (Vector.Vector v a) => (accum -> a -> Maybe accum) -> accum
    -> v a -> accum
fold_abort f accum vec = go 0 accum
    where go i accum = maybe accum (go (i+1)) $ f accum =<< vec Vector.!? i

-- | Find the index of the last value whose running sum is still below the
-- given number.
find_before :: (Vector.Vector v Int) => Int -> v Int -> Int
find_before n = fst . fold_abort go (0, 0)
    where
    go (i, total) a
        | total + a <= n = Just (i+1, total+a)
        | otherwise = Nothing
