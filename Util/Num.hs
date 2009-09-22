module Util.Num where

-- | Restrict a value to be between @low@ and @high@.
clamp :: (Ord a) => a -> a -> a -> a
clamp low high = min high . max low

-- | Scale @v@, which is between 0 and 1 inclusive, to be between @low@ and
-- @high@.
scale :: (Fractional a, Real b) => a -> a -> b -> a
scale low high v = realToFrac v * (high-low) + low

-- | Normalize @v@, which is between @low@ and @high@ inclusive, to be between
-- 0 and 1.
normalize :: (Real a, Fractional a, Fractional b) => a -> a -> a -> b
normalize low high v = realToFrac $ (v-low) / (high-low)
