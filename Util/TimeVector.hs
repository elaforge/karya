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
    module Util.TimeVector
    , module Util.TimeVectorStorable
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

to_foreign_ptr :: Storable.Storable a =>
    Storable.Vector a -> (Foreign.ForeignPtr a, Int)
to_foreign_ptr = Storable.unsafeToForeignPtr0

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

-- | Set the signal value, with a discontinuity.  See
-- NOTE [signal-discontinuity].
set :: V.Vector v (Sample y) => Maybe y -> X -> y -> v (Sample y)
set prev_y x y = from_pairs $ maybe id ((:) . (x,)) prev_y [(x, y)]

{-# SPECIALIZE constant :: UnboxedY -> Unboxed #-}
{-# INLINEABLE constant #-}
constant :: V.Vector v (Sample y) => y -> v (Sample y)
constant y = V.singleton (Sample (-RealTime.larger) y)

constant_val :: Unboxed -> Maybe UnboxedY
constant_val vec = case uncons vec of
    Nothing -> Just 0
    Just (Sample x0 y0, rest)
        -- I compare multiple samples because a track might have redundant
        -- values, but I still want to detect if it's constant.
        | x0 <= -RealTime.large && V.all ((==y0) . sy) rest -> Just y0
        | V.all ((==0) . sy) vec -> Just 0
        | otherwise -> Nothing

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

-- | This is a merge where the vectors to the right will win in the case of
-- overlap.
{-# SPECIALIZE merge_right :: [Unboxed] -> Unboxed #-}
{-# INLINEABLE merge_right #-}
merge_right :: V.Vector v (Sample y) => [v (Sample y)] -> v (Sample y)
merge_right [v] = v
merge_right vs = case next_start (reverse vs) of
    Nothing -> V.empty
    Just (v, vs, x) -> V.concat $ reverse $ v : trim x vs
    where
    -- I don't really like the double reverse, but it's easiest this way.
    trim prev_start (v : vs) =
        clipped : trim (maybe prev_start sx (head clipped)) vs
        where clipped = V.take (bsearch_below prev_start v) v
    trim _ [] = []
    next_start [] = Nothing
    next_start (v:vs) = maybe (next_start vs) (\s -> (Just (v, vs, sx s)))
        (head v)

-- | This is a merge where the vectors to the left will win in the case of
-- overlap.
{-# SPECIALIZE merge_left :: [Unboxed] -> Unboxed #-}
{-# INLINEABLE merge_left #-}
merge_left :: V.Vector v (Sample y) => [v (Sample y)] -> v (Sample y)
merge_left [v] = v
merge_left vs = case next_end vs of
    Nothing -> V.empty
    Just (v, vs, x) -> V.concat $ v : trim x vs
    where
    trim prev_end (v : vs) =
        clipped : trim (maybe prev_end sx (last clipped)) vs
        where clipped = V.dropWhile ((<=prev_end) . sx) v
    trim _ [] = []
    next_end [] = Nothing
    next_end (v:vs) = maybe (next_end vs) (\s -> (Just (v, vs, sx s))) (last v)
    -- |--->        => |--->
    --   |--->             |->
    --     |--->             |->

-- | When signals are 'merge_left'd, the later one overrides the first one.
-- This is the other way: the first one will override the second.
{-# SPECIALIZE prepend :: Unboxed -> Unboxed -> Unboxed #-}
{-# INLINEABLE prepend #-}
prepend :: V.Vector v (Sample y) => v (Sample y) -> v (Sample y)
    -> v (Sample y)
prepend vec1 vec2 = case last vec1 of
    Nothing -> vec2
    Just (Sample x _) ->
        vec1 V.++ V.dropWhile ((<=x) . sx) (drop_before_strict x vec2)

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

-- | Find the sample before the given X.
{-# SPECIALIZE before :: X -> Unboxed -> Maybe (Sample UnboxedY) #-}
{-# INLINEABLE before #-}
before :: V.Vector v (Sample y) => X -> v (Sample y) -> Maybe (Sample y)
before x vec
    | i > 0 = Just $ V.unsafeIndex vec (i-1)
    | otherwise = Nothing
    where i = bsearch_below x vec

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

{-# SPECIALIZE sig_op :: UnboxedY -> (UnboxedY -> UnboxedY -> UnboxedY)
    -> Unboxed -> Unboxed -> Unboxed #-}
{-# INLINEABLE sig_op #-}
-- | Combine two vectors with the given function.  They will be resampled so
-- they have samples at the same time.
sig_op :: V.Vector v (Sample y) =>
    y -- ^ The implicit y value of a vector before its first sample.  It should
    -- probably be the identity for the operator.
    -> (y -> y -> y) -> v (Sample y) -> v (Sample y) -> v (Sample y)
sig_op initial combine vec1 vec2 = V.unfoldr go (initial, initial, 0, 0)
    where
    go (prev_ay, prev_by, i1, i2) =
        case resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2 of
            Nothing -> Nothing
            Just (x, ay, by, i1, i2) ->
                Just (Sample x (combine ay by), (ay, by, i1, i2))
    len1 = V.length vec1
    len2 = V.length vec2

-- | Polymorphic variant of 'sig_op'.
--
-- The signature is specialized to Boxed since you might as well use 'sig_op'
-- for Unboxed vectors.
sig_op_poly :: y1 -> y2 -> (y1 -> y2 -> y3) -> Boxed y1 -> Boxed y2 -> Boxed y3
sig_op_poly initial1 initial2 combine vec1 vec2 =
    V.unfoldr go (initial1, initial2, 0, 0)
    where
    -- Yeah I could probably make 'sig_op' a specialization of this, but can't
    -- be bothered at the moment.
    go (prev_ay, prev_by, i1, i2) =
        case resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2 of
            Nothing -> Nothing
            Just (x, ay, by, i1, i2) ->
                Just (Sample x (combine ay by), (ay, by, i1, i2))
    len1 = V.length vec1
    len2 = V.length vec2

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

{-# SPECIALIZE find_nonascending :: Unboxed -> [(X, UnboxedY)] #-}
{-# INLINEABLE find_nonascending #-}
-- | Find samples whose 'sx' is <= the previous X.
find_nonascending :: V.Vector v (Sample y) => v (Sample y) -> [(X, y)]
find_nonascending vec = case uncons vec of
    Nothing -> []
    Just (x, xs) -> map to_pair $ reverse $ snd $ V.foldl' go (sx x, []) xs
    where
    go (prev, acc) s
        | sx s <= prev = (sx s, s : acc)
        | otherwise = (sx s, acc)

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

-- | 'bsearch_below', but if you use it with take, it includes the first
-- element ==x.  TODO not sure how to explain it.
{-# SPECIALIZE bsearch_below_1 :: X -> Unboxed -> Int #-}
{-# INLINEABLE bsearch_below_1 #-}
bsearch_below_1 :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
bsearch_below_1 x vec = case vec V.!? i of
    Just vi | sx vi == x -> i + 1
    _ -> i
    where i = bsearch_below x vec

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
