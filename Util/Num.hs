-- | Miscellaneous functions on numbers.  Things that could have gone in
-- Numeric.
module Util.Num where


-- | Restrict a value to be between @low@ and @high@.
clamp :: (Ord a) => a -> a -> a -> a
clamp low high = min high . max low

-- | Scale @v@, which is between 0 and 1 inclusive, to be between @low@ and
-- @high@.  If @v@ is not in the 0--1 range, the result will be out of the
-- low--high range.
scale :: (Fractional a, Real b) => a -> a -> b -> a
scale low high v = realToFrac v * (high-low) + low

-- | Normalize @v@, which is between @low@ and @high@ inclusive, to be between
-- 0 and 1.  As with 'scale', if @v@ is not in range, the result will not be
-- in range either.
normalize :: (Real a, Fractional a, Fractional b) => a -> a -> a -> b
normalize low high v
    | low == high && v == low = 0
    | otherwise = realToFrac $ (v-low) / (high-low)


double_to_float :: Double -> Float
double_to_float = realToFrac

float_to_double :: Float -> Double
float_to_double = realToFrac
