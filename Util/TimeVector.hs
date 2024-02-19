-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Generic functions over vectors of 'Sample's.

    The samples should be sorted, though this is currently not enforced by
    the constructors.  TODO fix that

    By default, this defines a piecewise-constant function, where each sample
    maintains its value until the next one.  However, samples can have
    coincident Xs, and this is used for a linear segment based implementation
    built on top of this.
-}
module Util.TimeVector (
    Boxed
    , Unboxed, UnboxedY
    , Sample(..), X
    , with_ptr
    , index
    , head, last, uncons
    , from_pairs, to_pairs, to_pair
    , at
    , constant
    , check
    , ascending, descending
    -- * transform
    , shift
    , drop_at_after
    , drop_before, drop_before_at, drop_before_strict
    , within
    , map_x, map_y
    , map_err
    , resample1
    -- * util
    , unfoldr
    , highest_index
    , index_below
    , x_at, y_at

    , module Data.Vector.Generic
) where
import           Prelude hiding (head, last, take)
import qualified Control.Monad.State.Strict as State
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as V
import           Data.Vector.Generic
    (all, drop, foldl', length, null, take, toList, unsafeIndex)
import qualified Data.Vector.Storable as Storable

import qualified Foreign
import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Util.Pretty as Pretty
import           Util.TimeVectorStorable (Sample(..), X)

import qualified Perform.RealTime as RealTime

import           Global


x_to_double :: X -> Double
x_to_double = RealTime.to_seconds

double_to_x :: Double -> X
double_to_x = RealTime.seconds

type Boxed y = Vector.Vector (Sample y)

-- * unboxed

-- A number of functions in here are SPECIALIZEd on Unboxed.  This improves
-- performance significantly since the functions are heavily used and the
-- specialization likely enables some unboxing in inner loops.

-- There's no monoid instance for Boxed or Unboxed, and I leave it that way.
-- Implementations should implement their own Monoid with their own rules,
-- perhaps using 'merge_left', which is for piecewise-constant signals.

type Unboxed = Storable.Vector (Sample UnboxedY)
type UnboxedY = Double

with_ptr :: Storable.Storable a =>
    Storable.Vector a -> (Foreign.Ptr a -> Int -> IO b) -> IO b
with_ptr v action = Storable.unsafeWith v $ \ptr -> action ptr (V.length v)

-- * implementation

index :: V.Vector v a => v a -> Int -> a
index = (V.!)

head, last :: V.Vector v a => v a -> Maybe a
head v
    | V.null v = Nothing
    | otherwise = Just $ V.unsafeIndex v 0
last v
    | V.null v = Nothing
    | otherwise = Just $ V.last v

uncons :: V.Vector v a => v a -> Maybe (a, v a)
uncons v
    | V.null v = Nothing
    | otherwise = Just (V.unsafeHead v, V.unsafeTail v)

-- ** TimeVector specific

-- | Construct a TimeVector from a list.
{-# SPECIALIZE from_pairs :: [(X, UnboxedY)] -> Unboxed #-}
{-# INLINEABLE from_pairs #-}
from_pairs :: V.Vector v (Sample y) => [(X, y)] -> v (Sample y)
from_pairs = V.fromList . map (uncurry Sample)

to_pairs :: V.Vector v (Sample y) => v (Sample y) -> [(X, y)]
to_pairs = map to_pair . V.toList

{-# SPECIALIZE constant :: UnboxedY -> Unboxed #-}
{-# INLINEABLE constant #-}
constant :: V.Vector v (Sample y) => y -> v (Sample y)
constant y = V.singleton (Sample (-RealTime.larger) y)

to_pair :: Sample y -> (X, y)
to_pair (Sample x y) = (x, y)

instance Pretty y => Pretty (Sample y) where
    format (Sample x y) = Pretty.format x <> Pretty.char ':' <> Pretty.format y

-- | TimeVectors should be sorted by the X value.  Return warnings for where
-- that's not true.
{-# SPECIALIZE check :: Unboxed -> [String] #-}
{-# INLINEABLE check #-}
check :: V.Vector v (Sample y) => v (Sample y) -> [String]
check = reverse . fst . V.foldl' check ([], (0, 0))
    where
    check (warns, (i, prev_x)) (Sample x _)
        | x < prev_x = first (msg:) next
        | otherwise = next
        where
        msg = "index " <> show i <> ": x decreased: " <> show x <> " < "
            <> show prev_x
        next = (warns, (i + 1, x))

-- | Same as 'sample_at', except don't return the X.
{-# SPECIALIZE at :: X -> Unboxed -> Maybe UnboxedY #-}
{-# INLINEABLE at #-}
at :: V.Vector v (Sample y) => X -> v (Sample y) -> Maybe y
at x = fmap snd . sample_at x

-- | Find the sample at or before X.  Nothing if the X is before the first
-- sample.
{-# SPECIALIZE sample_at :: X -> Unboxed -> Maybe (X, UnboxedY) #-}
{-# INLINEABLE sample_at #-}
sample_at :: V.Vector v (Sample y) => X -> v (Sample y) -> Maybe (X, y)
sample_at x vec
    | i >= 0 = Just $ to_pair $ V.unsafeIndex vec i
    | otherwise = Nothing
    where i = highest_index x vec

-- | Samples at and above the given time.
ascending :: V.Vector v (Sample y) => X -> v (Sample y) -> [Sample y]
ascending x vec =
    [ V.unsafeIndex vec i
    | i <- Lists.range' (bsearch_below x vec) (V.length vec) 1
    ]

-- | Descending samples, starting below the time.
descending :: V.Vector v (Sample y) => X -> v (Sample y) -> [Sample y]
descending x vec =
    [V.unsafeIndex vec i | i <- Lists.range (bsearch_below x vec - 1) 0 (-1)]

-- * transform

-- | Shift the signal in time.
shift :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
shift offset vec
    | offset == 0 = vec
    | otherwise = map_x (+offset) vec

-- | Truncate a signal so it doesn't include the given X - RealTime.eta.  It's
-- just a view of the old signal, so it doesn't allocate a new signal.
{-# SPECIALIZE drop_at_after :: X -> Unboxed -> Unboxed #-}
{-# INLINEABLE drop_at_after #-}
drop_at_after :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_at_after x vec = V.take (bsearch_below (x - RealTime.eta) vec) vec

drop_after :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_after x = drop_at_after (x + RealTime.eta + RealTime.eta)

-- | Like 'drop_before_strict', except if there is no sample at @x@, keep one
-- sample before it to preserve the value at @x@.  If there are multiple
-- samples at @x@, drop all but the last one.  This is because they indicate
-- a discontinuity, but if you don't care about the previous value, then you
-- don't need the discontinuity.
{-# SPECIALIZE drop_before :: X -> Unboxed -> Unboxed #-}
{-# INLINEABLE drop_before #-}
drop_before :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_before x vec
    | i == -1 = vec
    | i < V.length vec = V.drop i vec
    | otherwise = V.drop (V.length vec - 1) vec
    where i = highest_index x vec

-- | The reverse of 'drop_at_after': trim a signal's head up until, but not
-- including, the given X.
drop_before_strict :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_before_strict x vec = V.drop (bsearch_below x vec) vec

-- | Like 'drop_before_strict', but also drop samples at the X.
drop_before_at :: V.Vector v (Sample y) => X -> v (Sample y) -> v (Sample y)
drop_before_at x = V.dropWhile ((<=x) . sx) . drop_before_strict x

-- | Return samples to set the value at start and until end.  This means
-- samples start <= t < end, along with one < start if necessary to set
-- the initial value, and the end sample if start == end.
within :: V.Vector v (Sample y) => X -> X -> v (Sample y) -> v (Sample y)
within start end = (if start == end then drop_after end else drop_at_after end)
    . drop_before start

map_x :: V.Vector v (Sample y) => (X -> X) -> v (Sample y) -> v (Sample y)
map_x f = V.map $ \(Sample x y) -> Sample (f x) y

map_y :: V.Vector v (Sample y) => (y -> y) -> v (Sample y) -> v (Sample y)
map_y f = V.map $ \(Sample x y) -> Sample x (f y)

{-# SPECIALIZE map_err :: (Sample UnboxedY -> Either err (Sample UnboxedY))
    -> Unboxed -> (Unboxed, [err]) #-}
{-# INLINEABLE map_err #-}
-- | A map that can return error msgs.
map_err :: V.Vector v a => (a -> Either err a) -> v a -> (v a, [err])
map_err f vec = second reverse $ State.runState (V.mapM go vec) []
    where
    go sample =
        either (\err -> State.modify (err:) >> return sample) return (f sample)

{-# INLINE resample1 #-}
resample1 :: (V.Vector v1 (Sample y1), V.Vector v2 (Sample y2)) => y1 -> y2
    -> Int -> Int -> Int -> Int
    -> v1 (Sample y1) -> v2 (Sample y2) -> Maybe (X, y1, y2, Int, Int)
resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2
    | i1 >= len1 && i2 >= len2 = Nothing
    | i1 >= len1 = Just (bx, prev_ay, by, i1, i2+1)
    | i2 >= len2 = Just (ax, ay, prev_by, i1+1, i2)
    | ax == bx = Just (ax, ay, by, i1+1, i2+1)
    | ax < bx = Just (ax, ay, prev_by, i1+1, i2)
    | otherwise = Just (bx, prev_ay, by, i1, i2+1)
    where
    Sample ax ay = V.unsafeIndex vec1 i1
    Sample bx by = V.unsafeIndex vec2 i2

-- * util

{-# SPECIALIZE unfoldr :: (state -> Maybe ((X, UnboxedY), state)) -> state
    -> Unboxed #-}
{-# INLINEABLE unfoldr #-}
unfoldr :: V.Vector v (Sample y) => (state -> Maybe ((X, y), state)) -> state
    -> v (Sample y)
unfoldr f = V.unfoldr $ \st -> case f st of
    Nothing -> Nothing
    Just ((x, y), next) -> Just (Sample x y, next)

-- | Given a line defined by the two points, find the y at the given x.
-- Crashes if called on a vertical line (y0==y1).  Yeah, it's inconsistent
-- with 'x_at'.
y_at :: HasCallStack => X -> Double -> X -> Double -> X -> Double
y_at x0 y0 x1 y1 x
    | x0 == x1 = errorStack $ "y_at on vertical line: "
        <> showt ((x0, y0), (x1, y1), x)
    | otherwise = (y1 - y0) / x_to_double (x1 - x0) * x_to_double (x - x0) + y0

-- | Given a line defined by the two points, find the x at the given y.
x_at :: X -> Double -> X -> Double -> Double -> Maybe X
x_at x0 y0 x1 y1 y
    | y0 == y1 = Nothing -- line is horizontal
    | otherwise = Just $
        double_to_x (y - y0) / (double_to_x (y1 - y0) / (x1 - x0)) + x0

-- | Binary search for the highest index of the given X.  So the next value is
-- guaranteed to be >X, if it exists.  Return -1 if @x@ is before
-- the first element.  'RealTime.eta' is added to @x@, so a sample that's
-- almost the same will still be considered a match.
{-# SPECIALIZE highest_index :: X -> Unboxed -> Int #-}
{-# SPECIALIZE highest_index :: X -> Boxed y -> Int #-}
{-# INLINEABLE highest_index #-}
highest_index :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
highest_index x vec
    | V.null vec = -1
    | otherwise = i - 1
    where i = bsearch_above (x + RealTime.eta) vec

-- | Search for the last index <x, or -1 if the first sample is already >x.
{-# SPECIALIZE index_below :: X -> Unboxed -> Int #-}
{-# INLINEABLE index_below #-}
index_below :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
index_below x vec
    | i == 0 = case vec V.!? i of
        Just (Sample x1 _) | x1 == x -> 0
        _ -> -1
    | otherwise = i - 1
    where i = bsearch_below x vec

-- | Binary search for the index of the first element that is >x, or one past
-- the end of the vector.
{-# SPECIALIZE bsearch_above :: X -> Unboxed -> Int #-}
{-# SPECIALIZE bsearch_above :: X -> Boxed y -> Int #-}
{-# INLINEABLE bsearch_above #-}
bsearch_above :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
bsearch_above x vec = go 0 (V.length vec)
    where
    go low high
        | low == high = low
        | x >= sx (V.unsafeIndex vec mid) = go (mid+1) high
        | otherwise = go low mid
        where mid = (low + high) `div` 2

-- | Binary search for the index of the first element ==x, or the last one <x.
-- So it will be <=x, or one past the end of the vector.  If you ues it with
-- take, it's everything <x.
{-# SPECIALIZE bsearch_below :: X -> Unboxed -> Int #-}
{-# SPECIALIZE bsearch_below :: X -> Boxed y -> Int #-}
{-# INLINEABLE bsearch_below #-}
bsearch_below :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
bsearch_below x vec = go 0 (V.length vec)
    where
    go low high
        | low == high = low
        | x <= sx (V.unsafeIndex vec mid) = go low mid
        | otherwise = go (mid+1) high
        where mid = (low + high) `div` 2
