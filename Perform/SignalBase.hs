{-# LANGUAGE FlexibleContexts #-}
{- | 
-}
module Perform.SignalBase where
import qualified Control.Arrow as Arrow
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.StorableVector as V
import qualified Foreign.Storable as Storable

import Ui

import qualified Util.Seq as Seq

-- * types

-- | This is just a class alias to stave off huge ugly signatures.
--
-- A Signal is pairs of (x, y).  The X is always TrackPos, but the Y may be
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
    y_at :: X -> y -> X -> y -> X -> y
    -- | Given two Ys and a Double between 0 and 1, make a Y in between.
    project :: Double -> Double -> Double -> y

type X = TrackPos

signal :: (Signal y) => [(X, y)] -> SigVec y
signal xs@((x, _):_)
    | x == 0 = V.pack xs
    | otherwise = V.pack ((0, zero_y) : xs)
signal [] = V.pack []

unsignal :: (Signal y) => SigVec y -> [(X, y)]
unsignal = V.unpack

-- * constants

-- | Used to create a segment that continues \"forever\".
max_x :: X
max_x = TrackPos (2^52 - 1) -- 52 bits of mantissa only should be enough

-- Later, this should be under deriver control.
default_srate :: X
default_srate = TrackPos 0.05

-- * access

at :: (Signal y) => X -> SigVec y -> y
at x vec
    | i >= 0 = snd (V.index vec i)
    | not (V.null vec) = snd (V.index vec 0)
    | otherwise = zero_y
    where i = highest_index x vec

-- | 'at' with linear interpolation.
at_linear :: (Signal y) => X -> SigVec y -> y
at_linear x vec = interpolate x vec (highest_index x vec)
    where
    interpolate x vec i
        | V.null vec = zero_y
        | i + 1 >= V.length vec = y0
        | i < 0 = snd (V.index vec 0)
        | otherwise = y_at x0 y0 x1 y1 x
        where
        (x0, y0) = V.index vec i
        (x1, y1) = V.index vec (i+1)


-- | Return the highest index of the given X.  So the next value is
-- guaranteed to have a higher x, if it exists.  Return -1 if @x@ is before
-- the first element.
highest_index :: (Signal y) => X -> SigVec y -> Int
highest_index x vec
    | V.length vec == 0 = -1
    | otherwise = i - 1
    where i = bsearch_above vec fst x

-- | Generate samples starting at a certain point in the signal.
sample :: (Signal y) => X -> SigVec y -> [(X, y)]
sample start vec
    | V.null vec = [(start, zero_y)]
    | V.null rest = [(start, snd (V.index vec (V.length vec - 1)))]
    | otherwise = Seq.drop_initial_dups fst (V.unpack rest)
    where rest = V.drop (bsearch_on vec fst start) vec

-- | Find the index of the first element >= the key of the given element.
bsearch_on :: (Storable.Storable y, Ord key) =>
    V.Vector y -> (y -> key) -> key -> Int
bsearch_on vec key v = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | v <= key (vec `V.index` mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

-- | This gets the index of the value *after* @v@.
bsearch_above :: (Storable.Storable y, Ord key) =>
    V.Vector y -> (y -> key) -> key -> Int
bsearch_above vec key v = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | v >= key (V.index vec mid) = go vec (mid+1) high
        | otherwise = go vec low mid
        where mid = (low + high) `div` 2


-- * track signal

-- | Convert the track-level representation of a signal to a Signal.
track_signal :: (Signal y) => X -> [Segment] -> SigVec y
track_signal srate segs = signal (concat pairs)
    where
    pairs = case segs of
        (x, _, y) : rest | x == 0 ->
            [(0, project y y 0)] : (snd $ List.mapAccumL go (0, y) rest)
        _ -> snd $ List.mapAccumL go (0, 0) segs
    go (x0, y0) (x1, meth, y1) = ((x1, y1), samples)
        where samples = sample_track_seg srate x0 y0 x1 y1 meth

type Segment = (X, Method, Double)

-- | This corresponds to the methods allowed on controller track values.
--
-- TODO I think this will be too inflexible because it makes it hard to add
-- new methods.  It will probably turn into a more flexible data structure when
-- I start wanting those.
data Method =
    -- | Set the value at the given point in time.  The "to val" is ignored.
    Set
    -- | Approach the point with a straight line.
    | Linear
    -- | Approach the point with an exponential curve.  If the exponent is
    -- positive, the value will be x**n.  If it's negative, it will be
    -- x**(1/n).
    | Exp Double
    deriving (Show, Eq)

lin_function :: Double -> Double
lin_function x = x

exp_function :: Double -> Double -> Double
exp_function n x = x**exp
    where exp = if n >= 0 then n else (1 / abs n)

sample_track_seg :: (Y y) => X -> X -> Double -> X -> Double -> Method
    -> [(X, y)]
sample_track_seg srate x0 y0 x1 y1 meth = case meth of
    Set -> [(x1, project y1 y1 0)]
    Linear
        | y0 == y1 -> [(x1, project y1 y1 0)]
        | otherwise -> sample_function lin_function srate x0 y0 x1 y1
    Exp n -> sample_function (exp_function n) srate x0 y0 x1 y1

sample_function :: (Y y) => (Double -> Double) -> X
    -> X -> Double -> X -> Double -> [(X, y)]
sample_function f srate x0 y0 x1 y1 = zip xs (map (project y0 y1 . f) points)
    where
    xs = drop 1 (range True x0 x1 srate)
    points = map (\p -> realToFrac ((p-x0) / (x1-x0))) xs

-- | Like enumFromTo except it can include the final value.  Uses
-- multiplication instead of successive addition to avoid loss of precision.
range :: (Num a, Ord a) => Bool -> a -> a -> a -> [a]
range include_final start end step = go 0
    where
    go i
        | val >= end = if include_final then [end] else []
        | otherwise = val : go (i+1)
        where val = start + (i*step)

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

-- | Shift the signal in time.
shift :: (Signal y) => X -> SigVec y -> SigVec y
shift offset vec
    | offset == 0 = vec
    | otherwise = map_x (+offset) vec

-- | Truncate a signal.  It's just a view of the old signal, so it
-- doesn't allocate a new signal.
truncate :: (Signal y) => X -> SigVec y -> SigVec y
truncate x vec = fst $ V.splitAt (bsearch_on vec fst x) vec

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
    (_, above) = V.splitAt (bsearch_on vec fst start) vec
    (inside, _) = V.splitAt (bsearch_on above fst end) above
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
