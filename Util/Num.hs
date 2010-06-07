{-# LANGUAGE MagicHash #-}
-- | Miscellaneous functions on numbers.  Things that could have gone in
-- Numeric.
module Util.Num where
import qualified GHC.Types as Types
import qualified GHC.Prim as Prim
import qualified Foreign.C as C
import qualified Unsafe.Coerce as Coerce


-- | Restrict a value to be between @low@ and @high@.
clamp :: (Ord a) => a -> a -> a -> a
clamp low high = min high . max low

-- | Scale @v@, which is between 0 and 1 inclusive, to be between @low@ and
-- @high@.  If @v@ is not in the 0--1 range, the result will be out of the
-- low--high range.
scale :: (Num a) => a -> a -> a -> a
scale low high v = v * (high-low) + low

-- | Normalize @v@, which is between @low@ and @high@ inclusive, to be between
-- 0 and 1.  As with 'scale', if @v@ is not in range, the result will not be
-- in range either.
normalize :: (Fractional a) => a -> a -> a -> a
normalize low high v
    | low == high && v == low = 0
    | otherwise = (v-low) / (high-low)


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
