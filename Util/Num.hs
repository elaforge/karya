-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE MagicHash #-}
-- | Miscellaneous functions on numbers.  Things that could have gone in
-- Numeric.
module Util.Num where
import qualified Data.Bits as Bits
import qualified Data.Fixed as Fixed
import qualified GHC.Prim as Prim
import qualified GHC.Types as Types
import qualified Numeric


-- * show

-- | Show a word as binary.
--
-- Warning: for some reason, Integer is an instance of Bits, but bitSize will
-- crash.
binary :: (Bits.Bits a) => a -> String
binary b = map extract [bits-1, bits-2 .. 0]
    where
    extract i = if Bits.testBit b i then '1' else '0'
    bits = Bits.bitSize b

hex :: (Integral a, Show a) => a -> String
hex n = Numeric.showHex n ""

show_higit :: Int -> Maybe Char
show_higit c = case c of
    0 -> Just '0'; 1 -> Just '1'; 2 -> Just '2'; 3 -> Just '3'
    4 -> Just '4'; 5 -> Just '5'; 6 -> Just '6'; 7 -> Just '7'
    8 -> Just '8'; 9 -> Just '9'; 10 -> Just 'a'; 11 -> Just 'b'
    12 -> Just 'c'; 13 -> Just 'd'; 14 -> Just 'e'; 15 -> Just 'f'
    _ -> Nothing

-- * read

read_digit :: Char -> Maybe Int
read_digit c = case c of
    '0' -> Just 0; '1' -> Just 1; '2' -> Just 2; '3' -> Just 3; '4' -> Just 4
    '5' -> Just 5; '6' -> Just 6; '7' -> Just 7; '8' -> Just 8; '9' -> Just 9
    _ -> Nothing

-- * transform

-- | Clamp a value to be between @low@ and @high@.
clamp :: (Ord a) => a -> a -> a -> a
clamp low high = min high . max low

-- | Confine the given value lie between the first two arguments, but using
-- modulus, not clamping.
restrict :: (Real a) => a -> a -> a -> a
restrict low high
    | high == low = const low -- avoid dividing by 0 in fmod
    | otherwise = (+low) . (`fmod` (high-low)) . subtract low

in_range :: (Ord a) => a -> a -> a -> Bool
in_range low high x = low <= x && x < high

-- | Scale @v@, which is between 0 and 1 inclusive, to be between @low@ and
-- @high@.  If @v@ is not in the 0--1 range, the result will be out of the
-- low--high range.
{-# INLINEABLE scale #-}
{-# SPECIALIZE scale :: Double -> Double -> Double -> Double #-}
scale :: (Eq a, Num a) => a -> a -> a -> a
scale low high v
    -- Some calls to scale are likely to have 0 or 1.  If low and high are
    -- complicated expressions its nice to avoid evaluating them.
    | v == 0 = low
    | v == 1 = high
    | otherwise = v * (high-low) + low

-- | Normalize @v@, which is between @low@ and @high@ inclusive, to be between
-- 0 and 1.  As with 'scale', if @v@ is not in range, the result will not be
-- in range either.
{-# INLINEABLE normalize #-}
{-# SPECIALIZE normalize :: Double -> Double -> Double -> Double #-}
normalize :: (Eq a, Fractional a) => a -> a -> a -> a
normalize low high v
    | low == high && v == low = 0 -- avoid a divide by zero
    | v == high = 1
    | otherwise = (v-low) / (high-low)

infixl 7 `fmod` -- match `mod`

-- | fmod is in a bizarre place.
{-# SPECIALIZE fmod :: Double -> Double -> Double #-}
fmod :: (Real a) => a -> a -> a
fmod = Fixed.mod'

-- | realToFrac doesn't preserve the special float values and is inefficient.
--
-- There are some RULEs for this, but they aren't reliable.
d2f :: Double -> Float
d2f (Types.D# d) = Types.F# (Prim.double2Float# d)

f2d :: Float -> Double
f2d (Types.F# f) = Types.D# (Prim.float2Double# f)

-- | Conversion that clamps at INT_MIN / INT_MAX.
d2i :: Double -> Int
d2i d = floor (clamp min_int max_int d)
    where
    max_int = fromIntegral (maxBound :: Int)
    min_int = fromIntegral (minBound :: Int)

-- * misc

-- | Figure out closest ratio to the given double, whose denominator is up
-- to the given precision.
ratio_of :: Int -> Double -> (Int, Int)
ratio_of precision d = (int * floor denom + round (frac * denom), floor denom)
    where
    denom = min_or_0 error_of 2 [3 .. fromIntegral precision]
    (int, frac) = properFraction d
    error_of denom = abs (frac - fromIntegral (round (frac * denom)) / denom)

-- | This is like 'Seq.minimum_on' except it returns as soon as it sees 0.
min_or_0 :: (Num key, Ord key) => (a -> key) -> a -> [a] -> a
min_or_0 key x xs
    | key x == 0 = x
    | otherwise = go (key x) x xs
    where
    go _ low [] = low
    go low_k low (x:xs)
        | k == 0 = x
        | k < low_k = go k x xs
        | otherwise = go low_k low xs
        where k = key x
