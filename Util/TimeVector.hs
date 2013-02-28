{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- | Generic functions over vectors of 'Sample's.

    The samples should be sorted, though this is currently not enforced by
    the constructors.

    The meaning of the samples is defined by the 'at' implementation.  Each
    sample starts at its time and extends up to, but not including, the next
    sample.  Therefore if there are multiple samples at the same time, only the
    last one is visible.

    There is a special rule that says a sample at <=0 is considered to extend
    backwards indefinitely.  So @at (-1) [(1, 1)]@ is 0, but @at (-1) [(0, 1)]@
    is 1.  This is weird and I've gone back and forth a couple times on the
    design so I should document how this came up.

    The idea is that some calls will move events back in time (e.g. grace
    notes).  If the call starts at 0, the events will start at negative time.
    Controls start at 0, so these events will have no pitch and no controls.
    Even if the pitch is set explicitly via 'constant', it still starts at 0.
    I could solve the problem with 'constant' by starting it at -bignum, but
    that doesn't help for control tracks, which still start at 0.

    Since tracks provide no way to say what the value should be before they
    start, I have to do something implicit (well, technically they could, but
    it seems awkward).  So the idea is that a sample at 0 gives the value for
    negative times as well.  Actually the implementation is <=0, since it seems
    weird that translating a signal back from 0 would change its meaning.  This
    still leaves a bit of weirdness where translating a signal back from
    positive to will change its definition when it crosses 0.  Hopefully this
    won't be a problem in practice.

    This solves the problem for 'constant' and for tracks in the same way, and
    doesn't result in any ugly arbitrary -bignum values suddenly showing up in
    signals.
-}
module Util.TimeVector (
    module Util.TimeVector
    , module Util.TimeVectorStorable
    , module Data.Vector.Generic
) where
import Prelude hiding (head, last, take)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.DList as DList
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as V
import Data.Vector.Generic (length, null, all, foldl', unsafeIndex)
import qualified Data.Vector.Storable as Storable
import qualified Foreign

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Perform.RealTime as RealTime
import Util.TimeVectorStorable (X, Sample(..))


x_to_double :: X -> Double
x_to_double = RealTime.to_seconds

double_to_x :: Double -> X
double_to_x = RealTime.seconds

-- | X is a double, and doubles are inexact.  Functions that take an X value
-- should operate on values within a range.
eta :: X
eta = 0.00000000000004

-- * boxed

type Boxed y = Vector.Vector (Sample y)

instance DeepSeq.NFData (Boxed y) where
    rnf v = v `seq` ()

-- * unboxed

-- A number of functions in here are SPECIALIZEd on Unboxed.  This improves
-- performance significantly since the functions are heavily used and the
-- specialization likely enables some unboxing in inner loops.

-- (Vector a) already has a monoid instance, so I can't make my own.
-- I tried making newtypes for Boxed and Unboxed, but couldn't then figure
-- out how to get the generic functions to apply to them.  So clients have to
-- implement Monoid themselves, using 'merge'.

type Unboxed = Storable.Vector (Sample Double)

instance DeepSeq.NFData Unboxed where
    rnf v = v `seq` ()

to_foreign_ptr :: (Storable.Storable a) =>
    Storable.Vector a -> (Foreign.ForeignPtr a, Int)
to_foreign_ptr = Storable.unsafeToForeignPtr0

with_ptr :: (Storable.Storable a) =>
    Storable.Vector a -> (Foreign.Ptr a -> IO b) -> IO b
with_ptr = Storable.unsafeWith

-- * implementation

index :: (V.Vector v a) => v a -> Int -> a
index = (V.!)

head, last :: (V.Vector v a) => v a -> Maybe a
head v
    | V.null v = Nothing
    | otherwise = Just $ V.unsafeIndex v 0
last v
    | V.null v = Nothing
    | otherwise = Just $ V.last v

viewL :: (V.Vector v a) => v a -> Maybe (a, v a)
viewL v
    | V.null v = Nothing
    | otherwise = Just (V.unsafeHead v, V.unsafeTail v)

-- ** TimeVector specific

-- | Construct a TimeVector from a list.
{-# INLINEABLE signal #-}
{-# SPECIALIZE signal :: [(X, Double)] -> Unboxed #-}
signal :: (V.Vector v (Sample y)) => [(X, y)] -> v (Sample y)
signal = V.fromList . map (uncurry Sample)

unsignal :: (V.Vector v (Sample y)) => v (Sample y) -> [(X, y)]
unsignal = map to_pair . V.toList

{-# INLINEABLE constant #-}
{-# SPECIALIZE constant :: Double -> Unboxed #-}
constant :: (V.Vector v (Sample y)) => y -> v (Sample y)
constant y = V.singleton (Sample 0 y)
    -- Constant starts at 0, as documented in the module haddock.

is_constant :: Unboxed -> Bool
is_constant vec = case viewL vec of
    Nothing -> True
    Just (Sample x0 y0, rest)
        | x0 <= 0 -> V.all ((==y0) . sy) rest
        | otherwise -> V.all ((==0) . sy) vec

to_pair :: Sample y -> (X, y)
to_pair (Sample x y) = (x, y)

instance (Pretty.Pretty y) => Pretty.Pretty (Sample y) where
    format (Sample x y) = Pretty.format x <> Pretty.char ':' <> Pretty.format y

-- | Merge a sorted list of vectors.  Samples are not interspersed, and if
-- the vectors overlap the later one wins.
merge :: (V.Vector v (Sample y)) => [v (Sample y)] -> v (Sample y)
merge vecs = V.unfoldrN len go vecs
    where
    -- This will be too big if there's lots of overlap.
    len = sum (map V.length vecs) + 1
    go [] = Nothing
    go [vec] = case viewL vec of
        Nothing -> Nothing
        Just (x, rest) -> Just (x, [rest])
    go (cur : vecs@(next : rest)) = case viewL cur of
        Nothing -> go vecs
        Just (Sample x y, cur_tl) -> case viewL next of
            Nothing -> go (cur : rest)
            Just (Sample next_x next_y, next_tl)
                | next_x <= x -> Just (Sample next_x next_y, next_tl : rest)
                | otherwise -> Just (Sample x y, cur_tl : vecs)

-- | Find the value of the signal at the X value.  Nothing if the X is before
-- the first sample.
--
-- There is a special rule that says a sample at <=0 is considered to extend
-- backwards indefinitely.  So @at (-1) [(1, 1)]@ is 0, but
-- @at (-1) [(0, 1)]@ is 1.  More documentation in the module haddock.
{-# INLINEABLE at #-}
{-# SPECIALIZE at :: X -> Unboxed -> Maybe Double #-}
at :: (V.Vector v (Sample y)) => X -> v (Sample y) -> Maybe y
at x vec
    | i >= 0 = Just $ sy (V.unsafeIndex vec i)
    | otherwise = case viewL vec of
        Just (Sample x y, _) | x <= 0 -> Just y
        _ -> Nothing
    where i = highest_index x vec

-- | Shift the signal in time.
shift :: (V.Vector v (Sample y)) => X -> v (Sample y) -> v (Sample y)
shift offset vec
    | offset == 0 = vec
    | otherwise = map_x (+offset) vec

take :: (V.Vector v a) => Int -> v a -> v a
take = V.take

-- | Truncate a signal so it doesn't include the given X.  It's just a view of
-- the old signal, so it doesn't allocate a new signal.
--
-- If the x<=0 the signal will still contain up to and including 0.  That's
-- because, as per the module haddock, a sample <=0 stands in for all values
-- <=0.
{-# SPECIALIZE drop_after :: X -> Unboxed -> Unboxed #-}
drop_after :: (V.Vector v (Sample y)) => X -> v (Sample y) -> v (Sample y)
drop_after x vec
    | x <= 0 = V.takeWhile ((<=0) . sx) vec
    | otherwise = V.take (lowest_index (x-eta) vec) vec

-- | The reverse of 'drop_after'.  Trim a signal's head up until, but not
-- including, the given X.  If there is no sample at @x@, keep one sample
-- before it to preserve the value at @x@.
--
-- As with 'drop_after', this doesn't do any copying.
{-# SPECIALIZE drop_before :: X -> Unboxed -> Unboxed #-}
drop_before :: (V.Vector v (Sample y)) => X -> v (Sample y) -> v (Sample y)
drop_before x vec
    | i < V.length vec && sx (V.unsafeIndex vec i) == x =
        snd $ V.splitAt i vec
    | otherwise = snd $ V.splitAt (i-1) vec
    where i = lowest_index x vec

-- | Return samples within a range.  This is a combination of 'drop_before'
-- and 'drop_after'.
within :: (V.Vector v (Sample y)) => X -> X -> v (Sample y) -> v (Sample y)
within start end = drop_after end . drop_before start

map_x :: (V.Vector v (Sample y)) => (X -> X) -> v (Sample y) -> v (Sample y)
map_x f = V.map $ \(Sample x y) -> Sample (f x) y

map_y :: (V.Vector v (Sample y)) => (y -> y) -> v (Sample y) -> v (Sample y)
map_y f = V.map $ \(Sample x y) -> Sample x (f y)

{-# SPECIALIZE sig_op :: Double -> (Double -> Double -> Double)
    -> Unboxed -> Unboxed -> Unboxed #-}
{-# INLINEABLE sig_op #-}
sig_op :: (V.Vector v (Sample y)) =>
    y -- ^ the implicit y value of a vector before its first sample
    -> (y -> y -> y) -> v (Sample y) -> v (Sample y) -> v (Sample y)
sig_op initial f vec1 vec2 = V.unfoldr go (initial, initial, 0, 0)
    where
    go (prev_ay, prev_by, i1, i2) =
        case resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2 of
            Nothing -> Nothing
            Just (x, ay, by, i1, i2) ->
                Just (Sample x (f ay by), (ay, by, i1, i2))
    len1 = V.length vec1
    len2 = V.length vec2

-- | Polymorphic variant of 'sig_op'.
--
-- The signature is specialized to Boxed since you might as well use 'sig_op'
-- for Unboxed vectors.
sig_op2 :: y1 -> y2 -> (y1 -> y2 -> y3)
    -> Boxed y1 -> Boxed y2 -> Boxed y3
sig_op2 initial1 initial2 f vec1 vec2 = V.unfoldr go (initial1, initial2, 0, 0)
    where
    -- Yeah I could probably make 'sig_op' a specialization of this, but can't
    -- be bothered at the moment.
    go (prev_ay, prev_by, i1, i2) =
        case resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2 of
            Nothing -> Nothing
            Just (x, ay, by, i1, i2) ->
                Just (Sample x (f ay by), (ay, by, i1, i2))
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

-- | Given a line defined by the two points, find the y at the given x.
-- Crashes if called on a vertical line (y0==y1).  Yeah, it's inconsistent
-- with 'x_at'.
y_at :: X -> Double -> X -> Double -> X -> Double
y_at x0 y0 x1 y1 x
    | x0 == x1 = error $ "y_at on vertical line: "
        ++ show ((x0, y0), (x1, y1), x)
    | otherwise = (y1 - y0) / x_to_double (x1 - x0) * x_to_double (x - x0) + y0

-- | Given a line defined by the two points, find the x at the given y.
x_at :: X -> Double -> X -> Double -> Double -> Maybe X
x_at x0 y0 x1 y1 y
    | y0 == y1 = Nothing -- line is horizontal
    | otherwise = Just $
        double_to_x (y - y0) / (double_to_x (y1 - y0) / (x1 - x0)) + x0

-- | Find the the index at the lowest index of the given X, or where it would
-- be if it were present.  So the next value is guaranteed to be >= the given
-- X.
{-# SPECIALIZE lowest_index :: X -> Unboxed -> Int #-}
lowest_index :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
lowest_index x vec = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | x <= sx (V.unsafeIndex vec mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

-- | Return the highest index of the given X.  So the next value is
-- guaranteed to have a higher x, if it exists.  Return -1 if @x@ is before
-- the first element.
{-# SPECIALIZE highest_index :: X -> Unboxed -> Int #-}
highest_index :: (V.Vector v (Sample y)) => X -> v (Sample y) -> Int
highest_index x vec
    | V.null vec = -1
    | otherwise = i - 1
    where i = bsearch_above x vec

-- | This gets the index of the value *after* @x@.
{-# SPECIALIZE bsearch_above :: X -> Unboxed -> Int #-}
bsearch_above :: (V.Vector v (Sample y)) => X -> v (Sample y) -> Int
bsearch_above x vec = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | x >= sx (V.unsafeIndex vec mid) = go vec (mid+1) high
        | otherwise = go vec low mid
        where mid = (low + high) `div` 2

concat_map_accum :: (V.Vector v (Sample y)) => y
    -> (accum -> X -> y -> X -> y -> (accum, [Sample y]))
    -- ^ Take the previous accum, previous x and y, and current x and y.
    -> (accum -> Sample y -> [Sample y])
    -- ^ Given the final @(accum, Sample)@, produce samples to append.
    -> accum -> v (Sample y) -> v (Sample y)
concat_map_accum zero f final accum vec = V.fromList (DList.toList result)
    where
    (last_accum, _, dlist) =
        V.foldl' go (accum, Sample 0 zero, DList.empty) vec
    end = if V.null vec then [] else final last_accum (V.last vec)
    result = dlist `DList.append` DList.fromList end
    go (accum, Sample x0 y0, lst) (Sample x1 y1) =
        (accum2, Sample x1 y1, lst `DList.append` DList.fromList samples)
        where (accum2, samples) = f accum x0 y0 x1 y1
