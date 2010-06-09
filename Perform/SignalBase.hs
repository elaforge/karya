{-# LANGUAGE FlexibleContexts #-}
{- | Functions common between the different signal types.
-}
module Perform.SignalBase where
import qualified Control.Arrow as Arrow
import qualified Data.DList as DList
import qualified Data.StorableVector as V
import qualified Data.StorableVector.Base as VectorBase
import qualified Foreign.Storable as Storable

import Ui

-- * types

-- | This is just a class alias to stave off huge ugly signatures.
--
-- A Signal is pairs of (x, y).  The X is always RealTime, but the Y may be
-- any type that implements 'Y'.
class (Storable.Storable (X, y), Y y) => Signal y

-- The 'Signal' class isn't really a signal, but is a shorthand for the
-- constraints on @y@.  If Signal was a signal then maybe I could put in
-- 'modify_vec'?  Then 'at' etc. are simple re-exports with a stricter type
-- sig.
-- class ATSignal s where
--     -- data (Storable.Storable (X, y), Y y) => YVal s
--     data YVal
--     modify_vec :: s -> (SigVec YVal -> SigVec YVal) -> s

-- | This is a strict vector for now, eventually I may want to switch this
-- to a lazy one.
type SigVec y = V.Vector (X, y)

class (Eq y) => Y y where
    zero_y :: y
    to_double :: y -> Double

type X = RealTime

signal :: (Signal y) => [(X, y)] -> SigVec y
signal = V.pack

unsignal :: (Signal y) => SigVec y -> [(X, y)]
unsignal = V.unpack

-- * constants

-- | Used to create a segment that continues \"forever\".
max_x :: X
max_x = RealTime (2^52 - 1) -- 52 bits of mantissa only should be enough

-- Later, this should be under deriver control.
default_srate :: X
default_srate = RealTime 0.05

-- * access

at :: (Signal y) => X -> SigVec y -> y
at x vec
    | i >= 0 = snd (V.index vec i)
    | not (V.null vec) = snd (V.index vec 0)
    | otherwise = zero_y
    where i = highest_index x vec

-- | 'at' with linear interpolation.
--
-- 'at_linear' always returns a double, because \"delayed interpolated\" values
-- ala PitchSignal don't really make sense when they themselves are
-- interpolated.
at_linear :: (Signal y) => X -> SigVec y -> Double
at_linear x vec = interpolate x vec (highest_index x vec)
    where
    interpolate x vec i
        | V.null vec = 0
        | i + 1 >= V.length vec = to_double y0
        | i < 0 = to_double (snd (V.index vec 0))
        | otherwise = y_at (x_to_double x0) (to_double y0)
            (x_to_double x1) (to_double y1) (x_to_double x)
        where
        (x0, y0) = V.index vec i
        (x1, y1) = V.index vec (i+1)

x_to_double :: RealTime -> Double
x_to_double (RealTime x) = x

-- | Return the highest index of the given X.  So the next value is
-- guaranteed to have a higher x, if it exists.  Return -1 if @x@ is before
-- the first element.
highest_index :: (Signal y) => X -> SigVec y -> Int
highest_index x vec
    | V.null vec = -1
    | otherwise = i - 1
    where i = bsearch_above vec x

-- | Return all samples at and after a certain point in the signal.
sample :: (Signal y) => X -> SigVec y -> [(X, y)]
sample start vec
    | V.null vec = [(start, zero_y)]
    | V.null rest = [(start, snd (V.index vec (V.length vec - 1)))]
    | otherwise = V.unpack rest
    where rest = V.drop (bsearch vec start) vec

-- | Find the index of the first element >= the key of the given element.
bsearch_on :: (Storable.Storable y, Ord key) =>
    V.Vector y -> (y -> key) -> key -> Int
bsearch_on vec key v = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | v <= key (VectorBase.unsafeIndex vec mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

-- | A version of 'bsearch_on' specialized to search X.  Profiling says
-- this gets called a lot and apparently the specialization makes a difference.
bsearch :: (Storable.Storable (X, y)) => V.Vector (X, y) -> X -> Int
bsearch vec v = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | v <= fst (VectorBase.unsafeIndex vec mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

-- | This gets the index of the value *after* @v@.
bsearch_above :: (Storable.Storable (X, y)) => V.Vector (X, y) -> X -> Int
bsearch_above vec v = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | v >= fst (VectorBase.unsafeIndex vec mid) = go vec (mid+1) high
        | otherwise = go vec low mid
        where mid = (low + high) `div` 2


-- * comparison

-- | Are the given signals equal within the given range?
--
-- Equal signals with samples in different places will compare not equal, but
-- oh well.  I could resample them, but for the moment I won't bother because
-- I think they're only likely to be equal if they are the exact same curve.
equal :: (Signal y) => X -> X -> SigVec y -> SigVec y -> Bool
equal start end sig0 sig1 =
    at start sig0 == at start sig1 && at end sig0 == at end sig1
    && within start end sig0 == within start end sig1


-- * transformation

-- | Merge a sorted list of vectors.  Samples are not interspersed, and if
-- the vectors overlap the later one wins.
merge :: (Signal y) => [SigVec y] -> SigVec y
merge vecs = fst $ V.unfoldrN len go vecs
    where
    -- This will be too big if there's lots of overlap, but I plan to switch
    -- to a lazy vector anyway.
    len = sum (map V.length vecs) + 1
    go [] = Nothing
    go [vec] = case V.viewL vec of
        Nothing -> Nothing
        Just (x, rest) -> Just (x, [rest])
    go (cur : vecs@(next : rest)) = case V.viewL cur of
        Nothing -> go vecs
        Just ((x, y), cur_tl) -> case V.viewL next of
            Nothing -> go (cur : rest)
            Just ((next_x, next_y), next_tl)
                | next_x <= x -> Just ((next_x, next_y), next_tl : rest)
                | otherwise -> Just ((x, y), cur_tl : vecs)

-- | Shift the signal in time.
shift :: (Signal y) => X -> SigVec y -> SigVec y
shift offset vec
    | offset == 0 = vec
    | otherwise = map_x (+offset) vec

-- | Truncate a signal.  It's just a view of the old signal, so it
-- doesn't allocate a new signal.
truncate :: (Signal y) => X -> SigVec y -> SigVec y
truncate x vec = fst $ V.splitAt (bsearch vec x) vec

-- | The dual of 'truncate'.  Trim a signal's head up until, but not including,
-- the given X.
shorten :: (Signal y) => X -> SigVec y -> SigVec y
shorten x vec = snd $ V.splitAt (bsearch vec x) vec

map_x :: (Signal y) => (X -> X) -> SigVec y -> SigVec y
map_x f = V.map (Arrow.first f)

map_y :: (Signal y) => (y -> y) -> SigVec y -> SigVec y
map_y f = V.map (Arrow.second f)

-- | Combine two signals with the given op.
sig_op :: (Signal v0, Signal v1) =>
    (v0 -> v1 -> v0) -> SigVec v0 -> SigVec v1 -> SigVec v0
sig_op op sig0 sig1 =
    -- This inefficiently unpacks to a list and back.  Later implement
    -- a resample that doesn't unpack.
    signal [(x, op y0 y1) | (x, y0, y1) <- resample_to_list sig0 sig1]


-- | Map a function across pairs of samples, threading an additional
-- accumulator through for state.  The function is passed the *previous* sample
-- along with the current one, so it should return samples based on the second
-- sample it receives (the sample "previous" to the first sample will be
-- @(0, zero_y)@).  The function returns a list of samples, so this is
-- also like concatMap.
--
-- TODO I should be able to do a faster version of this by working directly
-- with the pointers.
map_signal_accum :: (Signal y) =>
    (accum -> X -> y -> X -> y -> (accum, [(X, y)]))
    -- ^ Take the previous accum, previous x and y, and current x and y.
    -> (accum -> (X, y) -> [(X, y)])
    -- ^ Given the final @(accum, (x, y))@, produce samples to append.
    -> accum -> SigVec y -> SigVec y
map_signal_accum f final accum vec = signal (DList.toList result)
    where
    (last_accum, _, dlist) = V.foldl' go (accum, (0, zero_y), DList.empty) vec
    end = if V.null vec then [] else final last_accum (V.last vec)
    result = dlist `DList.append` DList.fromList end
    go (accum, (x0, y0), lst) (x1, y1) =
        (accum2, (x1, y1), lst `DList.append` DList.fromList samples)
        where (accum2, samples) = f accum x0 y0 x1 y1

map_signal :: (Signal y) => (X -> y -> X -> y -> [(X, y)]) -> SigVec y
    -> SigVec y
map_signal f = map_signal_accum go (\_ _ -> []) ()
    where go _ x0 y0 x1 y1 = ((), f x0 y0 x1 y1)

-- * misc

within :: (Signal y) => X -> X -> SigVec y -> SigVec y
within start end vec = V.drop extra inside
    where
    -- TODO use bsearch_above?
    (_, above) = V.splitAt (bsearch vec start) vec
    (inside, _) = V.splitAt (bsearch above end) above
    -- Otherwise concurrent samples confuse pitches_share.
    -- TODO leading concurrent samples could confuse other things, maybe put
    -- this in unpack?
    extra = if V.null inside then 0
        else case V.findIndex ((/= fst (V.head inside)) . fst) inside of
            Nothing -> V.length inside - 1
            Just i -> i - 1

-- | Resample the signals to have coincident sample points.
--
-- This emits a list to take advantage of laziness.  Later when signals are
-- lazy I should probably emit two signals.
resample_to_list :: (Signal y0, Signal y1) =>
    SigVec y0 -> SigVec y1 -> [(X, y0, y1)]
resample_to_list vec0 vec1 =
    resample zero_y zero_y (V.unpack vec0) (V.unpack vec1)

resample :: y0 -> y1 -> [(X, y0)] -> [(X, y1)] -> [(X, y0, y1)]
resample _ prev_by as [] = [(x, y, prev_by) | (x, y) <- as]
resample prev_ay _ [] bs = [(x, prev_ay, y) | (x, y) <- bs]
resample prev_ay prev_by as@((ax, ay) : rest_a) bs@((bx, by) : rest_b)
    | ax == bx = (ax, ay, by) : resample ay by rest_a rest_b
    | ax < bx = (ax, ay, prev_by) : resample ay prev_by rest_a bs
    | otherwise = (bx, prev_ay, by) : resample prev_ay by as rest_b



-- | Like enumFromTo except it can include the final value.  Uses
-- multiplication instead of successive addition to avoid loss of precision.
range :: (Num a, Ord a) => Bool -> a -> a -> a -> [a]
range include_final start end step = go 0
    where
    go i
        | val >= end = if include_final then [end] else []
        | otherwise = val : go (i+1)
        where val = start + (i*step)


-- | Given a line defined by the two points, find the y at the given x.
y_at :: Double -> Double -> Double -> Double -> Double -> Double
y_at x0 y0 x1 y1 x
    | x == x1 = y1 -- avoid zero length segments
    | otherwise = (y1 - y0) / (x1 - x0) * (x - x0) + y0

-- | Given a line defined by the two points, find the x at the given y.
x_at :: Double -> Double -> Double -> Double -> Double -> Double
x_at x0 y0 x1 y1 y
    | x0 == x1 = x1 -- zero width means vertical, which means it crosses here
    | y0 == y1 = error $ "x_at on flat line " ++ show ((x0, y0), (x1, y1), y)
    | otherwise = (y - y0) / ((y1 - y0) / (x1 - x0)) + x0
