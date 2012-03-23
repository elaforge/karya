{-# LANGUAGE TypeSynonymInstances, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{- | Generic functions over vectors of Samples which have a RealTime
    attribute.
-}
module Util.TimeVector (
    module Util.TimeVector
    , Sample(..)
    , module Data.Vector.Generic
) where
import Prelude hiding (head, last, truncate)
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
import Perform.SignalStorable (Sample(..))


type X = RealTime.RealTime

x_to_double :: X -> Double
x_to_double = RealTime.to_seconds

double_to_x :: Double -> X
double_to_x = RealTime.seconds

-- * boxed

type Boxed y = Vector.Vector (Sample y)

instance DeepSeq.NFData (Boxed y) where
    rnf v = v `seq` ()

-- * unboxed

-- (Vector a) already has a monoid instance, so I can't make my own.
-- I tried making newtypes for Boxed and Unboxed, but couldn't then figure
-- out how to get the generic functions to apply to them.

type Unboxed = Storable.Vector (Sample Double)

instance DeepSeq.NFData Unboxed where
    rnf v = v `seq` ()

to_foreign_ptr :: (Storable.Storable a) =>
    Storable.Vector a -> (Foreign.ForeignPtr a, Int)
to_foreign_ptr = Storable.unsafeToForeignPtr0

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

signal :: (V.Vector v (Sample y)) => [(X, y)] -> v (Sample y)
signal = V.fromList . map (uncurry Sample)

unsignal :: (V.Vector v (Sample y)) => v (Sample y) -> [(X, y)]
unsignal = map to_pair . V.toList

to_pair :: Sample y -> (X, y)
to_pair (Sample x y) = (x, y)

instance (Pretty.Pretty y) => Pretty.Pretty (Sample y) where
    format (Sample x y) =
        Pretty.format x <> Pretty.char ':' Pretty.<+> Pretty.format y

-- | Merge a sorted list of vectors.  Samples are not interspersed, and if
-- the vectors overlap the later one wins.
merge  :: (V.Vector v (Sample y)) => [v (Sample y)] -> v (Sample y)
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
at :: (V.Vector v (Sample y)) => X -> v (Sample y) -> Maybe y
at x vec
    | i >= 0 = Just $ sy (V.unsafeIndex vec i)
    | otherwise = Nothing
    where i = highest_index x vec

-- | Shift the signal in time.
shift :: (V.Vector v (Sample y)) => X -> v (Sample y) -> v (Sample y)
shift offset vec
    | offset == 0 = vec
    | otherwise = map_x (+offset) vec

-- | Truncate a signal.  It's just a view of the old signal, so it
-- doesn't allocate a new signal.
truncate :: (V.Vector v (Sample y)) => X -> v (Sample y) -> v (Sample y)
truncate x vec = fst $ V.splitAt (bsearch_x x vec) vec

-- | The dual of 'truncate'.  Trim a signal's head up until, but not including,
-- the given X.  If there is no sample at @x@, keep one sample before it to
-- preserve the value at @x@.
--
-- As with 'truncate', this doesn't do any copying.
drop_before :: (V.Vector v (Sample y)) => X -> v (Sample y) -> v (Sample y)
drop_before x vec
    | i < V.length vec && sx (V.unsafeIndex vec i) == x =
        snd $ V.splitAt i vec
    | otherwise = snd $ V.splitAt (i-1) vec
    where i = bsearch_x x vec

-- | Return samples within a range.  This is a combination of 'drop_before'
-- and 'truncate'.
within :: (V.Vector v (Sample y)) => X -> X -> v (Sample y) -> v (Sample y)
within start end = truncate end . drop_before start

map_x :: (V.Vector v (Sample y)) => (X -> X) -> v (Sample y) -> v (Sample y)
map_x f = V.map $ \(Sample x y) -> Sample (f x) y

map_y :: (V.Vector v (Sample y)) => (y -> y) -> v (Sample y) -> v (Sample y)
map_y f = V.map $ \(Sample x y) -> Sample x (f y)

sig_op :: (V.Vector v (Sample y)) =>
    y -> (y -> y -> y) -> v (Sample y) -> v (Sample y) -> v (Sample y)
sig_op zero f vec0 vec1 = V.fromList
    -- TODO This inefficiently unpacks to a list and back.  Later implement
    -- a resample that doesn't unpack.
    [Sample x (f y0 y1) | (x, y0, y1) <- resample_to_list zero vec0 vec1]

resample_to_list :: (V.Vector v (Sample y)) =>
    y -> v (Sample y) -> v (Sample y) -> [(X, y, y)]
resample_to_list zero vec0 vec1 =
    resample zero zero (unsignal vec0) (unsignal vec1)

resample :: y0 -> y1 -> [(X, y0)] -> [(X, y1)] -> [(X, y0, y1)]
resample _ prev_by as [] = [(x, y, prev_by) | (x, y) <- as]
resample prev_ay _ [] bs = [(x, prev_ay, y) | (x, y) <- bs]
resample prev_ay prev_by as@((ax, ay) : rest_a) bs@((bx, by) : rest_b)
    | ax == bx = (ax, ay, by) : resample ay by rest_a rest_b
    | ax < bx = (ax, ay, prev_by) : resample ay prev_by rest_a bs
    | otherwise = (bx, prev_ay, by) : resample prev_ay by as rest_b

-- ** unfinished

sig_op_fast :: (V.Vector v (Sample y)) =>
    (y -> y -> y) -> v (Sample y) -> v (Sample y) -> v (Sample y)
sig_op_fast f vec0 vec1 = V.zipWith merge r0 r1
    where
    (r0, r1) = resample_vectors vec0 vec1
    merge (Sample x y0) (Sample _ y1) = Sample x (f y0 y1)

resample_vectors :: (V.Vector v (Sample y)) =>
    v (Sample y) -> v (Sample y) -> (v (Sample y), v (Sample y))
resample_vectors vec0 vec1 = undefined

-- * util

-- | Given a line defined by the two points, find the y at the given x.
-- Crashes if called on a vertical line (y0==y1).
y_at :: X -> Double -> X -> Double -> X -> Double
y_at x0 y0 x1 y1 x
    | x0 == x1 = error $ "y_at on vertical line: "
        ++ show ((x0, y0), (x1, y1), x)
    | otherwise = (y1 - y0) / x_to_double (x1 - x0) * x_to_double (x - x0) + y0

-- | Given a line defined by the two points, find the x at the given y.
-- Crashes if called on a horizontal line (x0==x1).
x_at :: X -> Double -> X -> Double -> Double -> X
x_at x0 y0 x1 y1 y
    | y0 == y1 = error $ "x_at on horizontal line: "
        ++ show ((x0, y0), (x1, y1), y)
    | otherwise =
        double_to_x (y - y0) / (double_to_x (y1 - y0) / (x1 - x0)) + x0


-- | A version of 'bsearch_on' specialized to search X.  Profiling says
-- this gets called a lot and apparently the specialization makes a difference.
bsearch_x :: V.Vector v (Sample y) => X -> v (Sample y) -> Int
bsearch_x x vec = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | x <= sx (V.unsafeIndex vec mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

-- | Return the highest index of the given X.  So the next value is
-- guaranteed to have a higher x, if it exists.  Return -1 if @x@ is before
-- the first element.
highest_index :: (V.Vector v (Sample y)) => X -> v (Sample y) -> Int
highest_index x vec
    | V.null vec = -1
    | otherwise = i - 1
    where i = bsearch_above x vec

-- | This gets the index of the value *after* @x@.
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
    go (accum, (Sample x0 y0), lst) (Sample x1 y1) =
        (accum2, (Sample x1 y1), lst `DList.append` DList.fromList samples)
        where (accum2, samples) = f accum x0 y0 x1 y1
