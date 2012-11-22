{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
-- | Vector utilities.
module Util.Vector where
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Unboxed as Unboxed


count :: (Generic.Vector v a) => (a -> Bool) -> v a -> Int
count f = Generic.foldl' (\c a -> if f a then succ c else c) 0

fold_abort :: (Generic.Vector v a) => (accum -> a -> Maybe accum) -> accum
    -> v a -> accum
fold_abort f accum vec = go 0 accum
    where go i accum = maybe accum (go (i+1)) $ f accum =<< vec Generic.!? i

-- | Find the index of the last value whose running sum is still below the
-- given number.
find_before :: (Generic.Vector v Int) => Int -> v Int -> Int
find_before n = fst . fold_abort go (0, 0)
    where
    go (i, total) a
        | total + a <= n = Just (i+1, total+a)
        | otherwise = Nothing

bracketing :: Unboxed.Vector Double -> Double -> Maybe (Int, Double, Double)
bracketing vec a = case Generic.findIndex (>=a) vec of
    Just i
        | Unboxed.unsafeIndex vec i == a -> Just (i, a, a)
        | i > 0 ->
            Just (i-1, Unboxed.unsafeIndex vec (i-1), Unboxed.unsafeIndex vec i)
    _ -> Nothing
