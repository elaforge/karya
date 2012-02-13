{-# LANGUAGE MagicHash #-}
-- | Miscellaneous functions on numbers.  Things that could have gone in
-- Numeric.
module Util.Num where
import qualified Data.Fixed as Fixed
import qualified Foreign.C as C
import qualified GHC.Prim as Prim
import qualified GHC.Types as Types
import qualified Unsafe.Coerce as Coerce


-- | Clamp a value to be between @low@ and @high@.
clamp :: (Ord a) => a -> a -> a -> a
clamp low high = min high . max low

-- | Confine the given value lie between the first two arguments, but using
-- modulus, not clamping.
restrict :: (Real a) => a -> a -> a -> a
restrict low high
    | high == low = const low -- avoid dividing by 0 in fmod
    | otherwise = (+low) . (`fmod` (high-low)) . (subtract low)

in_range :: (Ord a) => a -> a -> a -> Bool
in_range low high x = low <= x && x < high

-- | Scale @v@, which is between 0 and 1 inclusive, to be between @low@ and
-- @high@.  If @v@ is not in the 0--1 range, the result will be out of the
-- low--high range.
scale :: (Eq a, Num a) => a -> a -> a -> a
scale low high v
    -- Some calls to scale are likely to have 0 or 1.  If low and high are
    -- complicated expressions its nice to avoid evaluating them.
    | v == 0 = low
    | v == 1 = high
    | otherwise = v * (high-low) + low
-- TODO SPECIALIZE on Double?

-- | Normalize @v@, which is between @low@ and @high@ inclusive, to be between
-- 0 and 1.  As with 'scale', if @v@ is not in range, the result will not be
-- in range either.
normalize :: (Eq a, Fractional a) => a -> a -> a -> a
normalize low high v
    | low == high && v == low = 0 -- avoid a divide by zero
    | v == high = 1
    | otherwise = (v-low) / (high-low)

infixl 7 `fmod` -- match `mod`

-- | fmod is in a bizarre place.
fmod :: (Real a) => a -> a -> a
fmod = Fixed.mod'

-- | realToFrac doesn't preserve the special float values and is inefficient.
d2f :: Double -> Float
d2f (Types.D# d) = Types.F# (Prim.double2Float# d)

f2d :: Float -> Double
f2d (Types.F# f) = Types.D# (Prim.float2Double# f)

-- | There are rule pragmas that do this, but I don't trust them to always
-- fire.
d2c :: Double -> C.CDouble
d2c = Coerce.unsafeCoerce

c2d :: C.CDouble -> Double
c2d = Coerce.unsafeCoerce

-- | Conversion that clamps at INT_MIN / INT_MAX.
d2i :: Double -> Int
d2i d = floor (clamp min_int max_int d)
    where
    max_int = fromIntegral (maxBound :: Int)
    min_int = fromIntegral (minBound :: Int)
