-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
-- HasCallStack is redundant only if -O is passed.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Miscellaneous functions on numbers.  Things that could have gone in
-- Numeric.
module Util.Num where
import           Prelude hiding (sum)
import qualified Control.Exception as Exception
import           Data.Bifunctor (second)
import qualified Data.Bits as Bits
import qualified Data.Fixed as Fixed
import qualified Data.List as List
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Builder as Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.RealFloat as Lazy.Builder.RealFloat

import qualified GHC.Prim as Prim
import qualified GHC.Types as Types
import qualified Numeric

import qualified GHC.Stack as Stack


-- * show

-- | Show a word as binary.
--
-- Warning: for some reason, Integer is an instance of Bits, but bitSize will
-- crash.
binary :: Bits.Bits a => a -> String
binary b = case Bits.bitSizeMaybe b of
    Nothing -> ""
    Just bits -> map extract [bits-1, bits-2 .. 0]
    where
    extract i = if Bits.testBit b i then '1' else '0'

hex :: (Integral a, Show a) => Int -> a -> Text
hex pad n = mconcat
    [ if n < 0 then "-" else ""
    , Text.replicate (pad - Text.length s) "0"
    , s
    ]
    where s = Text.pack $ Numeric.showHex (abs n) ""

showHigit :: Int -> Maybe Char
showHigit c = case c of
    0 -> Just '0'; 1 -> Just '1'; 2 -> Just '2'; 3 -> Just '3'
    4 -> Just '4'; 5 -> Just '5'; 6 -> Just '6'; 7 -> Just '7'
    8 -> Just '8'; 9 -> Just '9'; 10 -> Just 'a'; 11 -> Just 'b'
    12 -> Just 'c'; 13 -> Just 'd'; 14 -> Just 'e'; 15 -> Just 'f'
    _ -> Nothing

zeroPad :: Show a => Int -> a -> Text
zeroPad digits n = Text.replicate (digits - Text.length s) "0" <> s
    where s = Text.pack (show n)

-- | Display a float with the given precision, dropping trailing and leading
-- zeros.  Haskell requires a 0 before the decimal point, so this produces
-- non-Haskell numbers.
showFloat :: RealFloat a => Int -> a -> Text
showFloat precision = drop0 . showFloat0 (Just precision)
    where
    drop0 t
        | t == "0" = "0"
        | Just rest <- Text.stripPrefix "-0." t = "-." <> rest
        | Just rest <- Text.stripPrefix "0." t = Text.cons '.' rest
        | otherwise = t

-- | Fancy decimal float formatting.  Show up to the given precision, like
-- 'showFloat', unless it would be all 0s.  In that case, show the minimum
-- necessary to get to a non-zero.
showFloatP :: RealFloat a => Bool -> Int -> a -> Text
showFloatP leadingZero precision num
    | num == 0 = if isNegativeZero num then "-0" else "0"
    | Text.all (=='0') inPrecision =
        prependInt $ takeWhile1 (=='0') (strip0 frac)
    | otherwise = prependInt $ strip0 inPrecision
    where
    numS = showFloat0 Nothing num
    (int, frac) = second (Text.drop 1) $ Text.break (=='.') numS
    intS = (if leadingZero then id else drop0) int <> "."
    inPrecision = Text.take precision frac
    takeWhile1 f t = Text.take (1 + Text.length (Text.takeWhile f t)) t
    prependInt = Text.dropWhileEnd (=='.') . (intS<>)

    drop0 t
        | "-0" `Text.isPrefixOf` t = "-"
        | "0" `Text.isPrefixOf` t = ""
        | otherwise = t
    strip0 = Text.dropWhileEnd (=='0')

-- | Like 'showFloat', but use a leading 0, so haskell can parse it.
showFloat0 :: RealFloat a => Maybe Int -> a -> Text
showFloat0 precision =
    dropTrailing0 . Text.Lazy.toStrict . Text.Lazy.Builder.toLazyText
    . Lazy.Builder.RealFloat.formatRealFloat Lazy.Builder.RealFloat.Fixed
        precision
    where
    dropTrailing0
        | maybe True (>0) precision =
            Text.dropWhileEnd (=='.') . Text.dropWhileEnd (=='0')
        | otherwise = id

-- * assert

-- | Like @div@, but assert that it divides evenly.
assertDiv :: Stack.HasCallStack => Integral a => a -> a -> a
assertDiv a b = Exception.assert (a `mod` b == 0) $ a `div` b

assertIntegral :: (Stack.HasCallStack, RealFrac f, Integral i) => f -> i
assertIntegral frac = Exception.assert (f == 0) i
    where (i, f) = properFraction frac

-- * read

readDigit :: Char -> Maybe Int
readDigit c = case c of
    '0' -> Just 0; '1' -> Just 1; '2' -> Just 2; '3' -> Just 3; '4' -> Just 4
    '5' -> Just 5; '6' -> Just 6; '7' -> Just 7; '8' -> Just 8; '9' -> Just 9
    _ -> Nothing

-- * transform

-- | Prelude sum is lazy, for silly reasons.
sum :: (Foldable t, Num a) => t a -> a
sum = List.foldl' (+) 0

roundDigits :: (RealFrac a, Fractional b) => Int -> a -> b
roundDigits digits = (/ (10^digits)) . fromIntegral . round . (* (10^digits))

-- | Round up to the nearest factor above the given number.
roundUp :: (Integral factor, Real a) => factor -> a -> factor
roundUp factor n =
    ceiling (realToFrac n / fromIntegral (abs factor)) * abs factor
{-# INLINEABLE roundUp #-}

roundDown :: (Integral factor, Real a) => factor -> a -> factor
roundDown factor n =
    floor (realToFrac n / fromIntegral (abs factor)) * abs factor
{-# INLINEABLE roundDown #-}

roundDownD :: Double -> Double -> Double
roundDownD factor n = floorD (n / abs factor) * abs factor

roundToD :: Double -> Double -> Double
roundToD factor n = roundD (n / abs factor) * abs factor

-- | Clamp a value to be between @low@ and @high@.
clamp :: Ord a => a -> a -> a -> a
clamp low high = min high . max low

-- | Confine the given value lie between the first two arguments, but using
-- modulus, not clamping.
restrict :: Real a => a -> a -> a -> a
restrict low high
    | high == low = const low -- avoid dividing by 0 in fmod
    | otherwise = (+low) . (`fmod` (high-low)) . subtract low

inRange :: Ord a => a -> a -> a -> Bool
inRange low high x = low <= x && x < high

-- | Scale @v@, which is between 0 and 1 inclusive, to be between @low@ and
-- @high@.  If @v@ is not in the 0--1 range, the result will be out of the
-- low--high range.
--
-- This is linear interpolate, or lerp.
{-# INLINEABLE scale #-}
{-# SPECIALIZE scale :: Double -> Double -> Double -> Double #-}
{-# SPECIALIZE scale :: Float -> Float -> Float -> Float #-}
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
{-# SPECIALIZE normalize :: Float -> Float -> Float -> Float #-}
normalize :: (Eq a, Fractional a) => a -> a -> a -> a
normalize low high v
    | low == high && v == low = 0 -- avoid a divide by zero
    | v == high = 1
    | otherwise = (v-low) / (high-low)

infixl 7 `fmod` -- match `mod`

-- | fmod is in a bizarre place.
{-# SPECIALIZE fmod :: Double -> Double -> Double #-}
fmod :: Real a => a -> a -> a
fmod = Fixed.mod'

{-# SPECIALIZE fDivMod :: Double -> Double -> (Int, Double) #-}
fDivMod :: (Integral div, RealFrac mod) => mod -> mod -> (div, mod)
fDivMod a b = (floor (a / b), fmod a b)

integral :: RealFrac a => a -> Bool
integral = (==0) . snd . properFraction

asIntegral :: (RealFrac frac, Integral int) => frac -> Maybe int
asIntegral n
    | f == 0 = Just i
    | otherwise = Nothing
    where (i, f) = properFraction n

-- | realToFrac doesn't preserve the special float values and is inefficient.
--
-- There are some RULEs for this, but they aren't reliable.
d2f :: Double -> Float
d2f (Types.D# d) = Types.F# (Prim.double2Float# d)

f2d :: Float -> Double
f2d (Types.F# f) = Types.D# (Prim.float2Double# f)

-- | Conversion that clamps at INT_MIN / INT_MAX.
d2i :: Double -> Int
d2i d = floor (clamp minInt maxInt d)
    where
    maxInt = fromIntegral (maxBound :: Int)
    minInt = fromIntegral (minBound :: Int)

i2d :: Int -> Double
i2d = fromIntegral

-- Versions of floor and round that don't go through an int conversion.
foreign import ccall unsafe "floor" floorD :: Double -> Double
foreign import ccall unsafe "floorf" floorF :: Float -> Float
foreign import ccall unsafe "round" roundD :: Double -> Double
foreign import ccall unsafe "roundf" roundF :: Float -> Float
