{-# LANGUAGE FlexibleInstances #-}
{- | This module implements signals as sparse arrays of Val->Val.  The
    points are interpolated linearly, so the signal array represents a series
    of straight line segments.

    There is an implicit initial sample at (0, 0).  The final sample is
    considered to extend in a flat line infinitely to the right.

    There are a few design trade offs here:

    1. Samples are stored as (x, y) pairs instead of having a constant sample
    rate.  This makes a lot of the functions in here much more complicated,
    but should result in a drastic reduction of data for the common case of
    long flat segments (e.g. constant tempo, constant controls esp. velocity).
    Also, a constant sample rate would restrict note resolution to the sample
    rate or you wouldn't be able to line them up.  A 1k sampling rate is
    already past human perception (and the midi driver's timing accuracy), but
    notes may be stretched in time, which will exacerbate any timing
    quantization.  Signal processing functions may resample the signal to raise
    the sampling rate, but shouldn't lower it, so if a signal is recorded with
    certain points, they should be played exactly as recorded even if they
    don't line up with the sampling rate.  TODO currently integrate doesn't do
    that, but I don't think it's too bad...

    2. Sample points are interpolated linearly rather than setting flat
    segments.  This means long linear ramps (such as the integral of a constant
    tempo) don't have to be sampled, which should be a big bonus.  However, it
    means that the common case of recorded midi controllers takes twice as much
    data, since a flat segment must be expressed as [(x0, y0), (x1, y0), (x2,
    y1), ...].  This will be bad for recorded midi controllers, but I may wind
    up with a special storage hack for those anyway.  Or maybe linear
    interpolation is ok for dense signal, if it's above the sampling rate then
    it doesn't matter anyway.

    3. Sample values are doubles, which means each point in the signal is 8*2
    bytes.  The double resolution is overkill for the value, but float would be
    too small for time given the time stretching mentioned above.

    Originally Signals were simply functions (Val -> Val).  This is much more
    elegant and things like composition are simply functional composition and
    hacks like shift and stretch go away.  Unfortunately, I need access to the
    points to draw graphs without resorting to sampling and things like
    integrate must be evaluated incrementally anyway, and I want to GC the
    heads of the signals when they are no longer needed, so...

    TODO

    - Make Signal polymorphic in Val so I can have Float for most things,
    Double for tempo warps, and (Octave, Degree, Offset) for pitches.  If
    a store as a pair of arrays then Float will take up 2/3 the space.

    - do some performance tests for large signals

    - implement a more efficient map_signal_accum and see if it helps
-}

module Perform.Signal where
import qualified Control.Arrow as Arrow
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.StorableVector as V
import qualified Foreign.Storable as Storable

import qualified Util.Seq as Seq

import Ui
import qualified Ui.Track as Track

import qualified Perform.Timestamp as Timestamp


-- * construction / deconstruction

-- | This is a strict vector for now, eventually I may want to switch this
-- to a lazy one.
data Signal = Signal SigVec deriving (Eq)
    -- The Eq instance is only for tests, since it may be quite expensive on
    -- a real signal.

-- | A pitch signal.  For the moment this is the same as a normal Signal since
-- scale degrees are reduced to absolute numbers during signal derivation, but
-- if I want to be able to handle relative pitch generically (i.e.
-- scale-independent transpositions and transpose non-tempered scales) this
-- will have to be something like [(TrackPos, Method, Perform.Pitch)].
type PitchSignal = Signal

type SigVec = V.Vector (Double, Double)

instance Storable.Storable (Double, Double) where
    sizeOf _ = Storable.sizeOf (undefined :: Double) * 2
    alignment _ = Storable.alignment (undefined :: Double)
    poke cp (a, b) = Storable.pokeByteOff cp 0 a >> Storable.pokeByteOff cp 8 b
    peek cp = do
        a <- Storable.peekByteOff cp 0 :: IO Double
        b <- Storable.peekByteOff cp 8 :: IO Double
        return (realToFrac a, realToFrac b)

instance Show Signal where
    show (Signal vec) = "Signal " ++ show (V.unpack vec)

type Val = Double
type Sample = (TrackPos, Val)

signal :: [(TrackPos, Val)] -> Signal
signal vals =
    Signal (V.pack (map (\(a, b) -> (pos_to_val a, b)) vals))

constant :: Val -> Signal
constant n = signal [(0, n)]

-- TODO unused but maybe useful some day
is_constant :: TrackPos -> TrackPos -> Signal -> Bool
is_constant start end sig =
    all (== (Seq.mhead 0 id vals)) vals && at start sig == at end sig
    where vals = unpack_vals (within start end sig)

unpack :: Signal -> [(TrackPos, Val)]
unpack (Signal vec) = map (Arrow.first val_to_pos) (V.unpack vec)

unpack_vals :: Signal -> [Val]
unpack_vals (Signal vec) = map snd (V.unpack vec)

to_track_samples :: Signal -> Track.Samples
to_track_samples = Track.samples . unpack


-- ** track signals

-- | This is how signal segments are represented on the track.
--
-- Each segment describes a point and how to /approach/ it from the previous
-- point.
type TrackSegment = (TrackPos, Method, Val)

data Method =
    -- | Set the value at the given point in time.  The "to Val" is ignored.
    Set
    -- | Approach the point with a straight line.
    | Linear
    -- | Approach the point with an exponential curve.  If the exponent is
    -- positive, the value will be pos**n.  If it's negative, it will be
    -- pos**(1/n).
    | Exp Double
    deriving (Show, Eq)

-- | Convert the track-level representation of a signal to a Signal.
track_signal :: TrackPos -> [TrackSegment] -> Signal
track_signal srate segs =
    Signal $ V.pack (map first_to_val (concat pairs))
    where
    (last_pair, pairs) = case segs of
        (x, meth, y) : rest | x == 0 -> List.mapAccumL go (x, y) segs
        _ -> List.mapAccumL go (0, 0) segs
    go (pos0, val0) (pos1, meth, val1) = ((pos1, val1), samples)
        where samples = sample_track_seg srate pos0 val0 pos1 val1 meth

sample_track_seg :: TrackPos -> TrackPos -> Val -> TrackPos -> Val -> Method
    -> [Sample]
sample_track_seg srate x0 y0 x1 y1 meth = case meth of
    Set -> [(x1, y1)]
    Linear
        | y0 == y1 -> [(x1, y1)]
        | otherwise -> sample_function (lin_function y0 y1) srate x0 x1
    Exp n -> sample_function (exp_function n y0 y1) srate x0 x1

sample_function :: (Double -> Double) -> TrackPos -> TrackPos -> TrackPos
    -> [Sample]
sample_function f srate start end = zip samples (map f points)
    where
    samples = drop 1 (range start end srate)
    points = map (\p -> realToFrac ((p-start) / (end-start))) samples

-- *** interpolation functions

-- 'amount' is called ranging from 0 to 1.

lin_function :: Val -> Val -> Double -> Val
lin_function y0 y1 amount = y0 + (y1-y0) * amount

exp_function :: Double -> Val -> Val -> Double -> Val
exp_function n y0 y1 amount
    | amount >= 1 = y1
    | otherwise = y0 + amount**exp * (y1 - y0)
    where exp = if n >= 0 then n else (1 / abs n)


-- * constants

-- | Used to create a segment that continues \"forever\".
max_track_pos :: TrackPos
max_track_pos = TrackPos (2^52 - 1) -- 52 bits of mantissa only should be enough

-- Later, this should be under deriver control.
default_srate :: TrackPos
default_srate = TrackPos 0.05

-- * comparison

-- | Are the given signals equal within the given range?
--
-- Equal signals with samples in different places will compare not equal, but
-- oh well.  I could resample them, but for the moment I won't bother because
-- I think they're only likely to be equal if they are the exact same curve.
equal :: TrackPos -> TrackPos -> Signal -> Signal -> Bool
equal start end sig0 sig1 =
    at start sig0 == at start sig1 && at end sig0 == at end sig1
    && within start end sig0 == within start end sig1

-- | Can the pitch signals share a channel within the given range?
--
-- Pitch is complicated.  Like other controllers, if the pitch curves are
-- different they may not share a channel.  However, if the pitch curves
-- are integral transpositions of each other, and the transposition is not
-- 0, they should share.  Unless the overlap occurs during the decay of one or
-- both notes, at which point 0 transposition is ok.
pitches_share :: Bool -> TrackPos -> TrackPos -> Signal -> Signal -> Bool
pitches_share in_decay start end sig0 sig1 =
    pitch_share in_decay (at start sig0) (at start sig1)
        && pitch_share in_decay (at end sig0) (at end sig1)
        && all pitch_eq samples
    where
    -- Unlike 'equal' I do resample, because there's a high chance of notes
    -- matching but not lining up in time.
    samples = resample_to_list (within start end sig0) (within start end sig1)
    pitch_eq (_, ay, by) = pitch_share in_decay ay by

-- | Only compare out to cents, since differences below that aren't really
-- audible.
pitch_share :: Bool -> Double -> Double -> Bool
pitch_share in_decay v0 v1 =
    (in_decay || fst (properFraction v0) /= fst (properFraction v1))
        && f v0 == f v1
    where f v = floor (snd (properFraction v) * 1000)

within :: TrackPos -> TrackPos -> Signal -> Signal
within start end (Signal vec) = Signal (V.drop extra inside)
    where
    (_, above) = V.splitAt (bsearch_on vec fst (pos_to_val start)) vec
    (inside, _) = V.splitAt (bsearch_on above fst (pos_to_val end)) above
    -- Otherwise concurrent samples confuse pitches_share.
    -- TODO leading concurrent samples could confuse other things, maybe put
    -- this in unpack?
    extra = if V.null inside then 0
        else case V.findIndex ((/= fst (V.head inside)) . fst) inside of
            Nothing -> V.length inside - 1
            Just i -> i - 1

-- * resample

-- | Resample the signals to have coincident sample points.
--
-- This emits a list to take advantage of laziness.  Later when signals are
-- lazy I should probably emit two signals.
resample_to_list :: Signal -> Signal -> [(Val, Val, Val)] -- ^ [(x, y0, y1)]
resample_to_list (Signal vec0) (Signal vec1) =
    _resample 0 0 (V.unpack vec0) (V.unpack vec1)

_resample :: Val -> Val -> [(Val, Val)] -> [(Val, Val)] -> [(Val, Val, Val)]
_resample _ prev_by as [] = [(x, y, prev_by) | (x, y) <- as]
_resample prev_ay _ [] bs = [(x, prev_ay, y) | (x, y) <- bs]
_resample prev_ay prev_by as@((ax, ay) : rest_a) bs@((bx, by) : rest_b)
    | ax == bx = (ax, ay, by) : _resample ay by rest_a rest_b
    | ax < bx = (ax, ay, prev_by) : _resample ay prev_by rest_a bs
    | otherwise = (bx, prev_ay, by) : _resample prev_ay by as rest_b


-- * access

at :: TrackPos -> Signal -> Val
at pos sig@(Signal vec) = case index pos sig of
    Just i | i >= 0 -> snd (V.index vec i)
    _ -> 0

-- | 'at' with linear interpolation.
at_linear :: TrackPos -> Signal -> Val
at_linear pos sig@(Signal vec) = maybe 0 interpolate (index pos sig)
    where
    interpolate i
        | i + 1 >= V.length vec = y0
        | otherwise = y_at x0 y0 x1 y1 (pos_to_val pos)
        where
        (x0, y0) = if i < 0 then (0, 0) else V.index vec i
        (x1, y1) = V.index vec (i+1)


-- | Return the highest index of the given TrackPos.  So the next value is
-- guaranteed to have a higher pos, if it exists.  Return -1 if @pos@ is before
-- the first element.
index :: TrackPos -> Signal -> Maybe Int
index pos (Signal vec)
    | V.length vec == 0 = Nothing
    | otherwise = Just (i-1)
    where
    i = bsearch_above vec fst (pos_to_val pos)
    -- | This gets the index of the value *after* @v@.
    bsearch_above :: SigVec -> ((Val, Val) -> Val) -> Val -> Int
    bsearch_above vec key v = go vec 0 (V.length vec)
        where
        go vec low high
            | low == high = low
            | v >= key (V.index vec mid) = go vec (mid+1) high
            | otherwise = go vec low mid
            where mid = (low + high) `div` 2

-- | Generate samples starting at a certain point in the signal.
--
-- This won't emit consecutive samples with the same value.  This might
-- cause problems if some MIDI instrument treats them as significant, but I'll
-- worry about that if I find one.
sample :: TrackPos -> Signal -> [Sample]
sample start (Signal vec)
    | V.null vec = [(start, 0)]
    | V.null rest = [(start, snd (V.index vec (V.length vec - 1)))]
    | otherwise = map first_to_pos $ Seq.drop_initial_dups fst (V.unpack rest)
    where rest = V.drop (bsearch_on vec fst (pos_to_val start)) vec

-- | Like enumFromTo except always include the final value.
-- Use multiplication instead of successive addition to avoid loss of precision.
range :: (Num a, Ord a) => a -> a -> a -> [a]
range start end step = go 0
    where
    go i
        | val >= end = [end]
        | otherwise = val : go (i+1)
        where val = start + (i*step)

-- ** implementation

-- | Find the index of the first element >= the key of the given element.
-- Signature is specialized to SigVec though it doesn't need to be.
bsearch_on :: SigVec -> ((Val, Val) -> Val) -> Val -> Int
bsearch_on vec key v = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | v <= key (vec `V.index` mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

-- * functions

-- ** signal ops

sig_add, sig_subtract :: Signal -> Signal -> Signal
sig_add = sig_op (+)
sig_subtract = sig_op (-)

sig_mult :: Signal -> Signal -> Signal
sig_mult = undefined -- will have to sample since lin*lin = exp

sig_max, sig_min :: Signal -> Signal -> Signal
sig_max = sig_op max
sig_min = sig_op min

sig_op :: (Val -> Val -> Val) -> Signal -> Signal -> Signal
sig_op op sig0 sig1 =
    -- This inefficiently unpacks to a list and back.  Later implement
    -- a resample that doesn't unpack.
    signal [(val_to_pos x, op y0 y1) | (x,y0,y1) <- resample_to_list sig0 sig1]

-- | Clip signal to never go above or below the given value.  Like 'sig_max'
-- and 'sig_min' except relative to a scalar value.
clip_max, clip_min :: Val -> Signal -> Signal
clip_max max_val (Signal vec) = Signal $ V.map (Arrow.second (min max_val)) vec
clip_min min_val (Signal vec) = Signal $ V.map (Arrow.second (max min_val)) vec

-- ** special functions

-- | Find the TrackPos at which the signal will attain the given Val.  Assumes
-- the Val is non-decreasing.
--
-- Unlike the other signal functions, this takes a single sample instead of
-- a signal, and as a Timestamp.  This is because it's used by the play updater
-- for the inverse tempo map, and the play updater doesn't necessarily poll at
-- totally regular intervals.
--
-- This uses a bsearch on the vector, which is only reasonable as long as
-- its strict.  When I switch to lazy vectors, I'll have to thread the tails.
inverse_at :: Signal -> Timestamp.Timestamp -> Maybe TrackPos
inverse_at (Signal vec) ts
    | i >= len = Nothing
    | y1 == y = Just (TrackPos x1)
    | otherwise = Just $ TrackPos $ x_at x0 y0 x1 y1 y
    where
    len = V.length vec
    y = pos_to_val (Timestamp.to_track_pos ts)
    i = bsearch_on vec snd y
        -- This can create x0==x1, but y1 should == y in that case.
    (x0, y0) = if i-1 < 0 then (0, 0) else V.index vec (i-1)
    (x1, y1) = V.index vec i

-- | Compose the first signal with the second.
compose :: Signal -> Signal -> Signal
compose f g = map_val go g
    where go y = at_linear (val_to_pos y) f
    -- TODO Walking down f would be more efficient, especially once Signal is
    -- lazy.

-- | Shift the signal in time.
shift :: TrackPos -> Signal -> Signal
shift offset sig
    | offset == 0 = sig
    | otherwise = map_pos (+offset) sig

stretch :: TrackPos -> Signal -> Signal
stretch mult sig
    | mult == 1 = sig
    | otherwise = map_pos (*mult) sig

-- | Integrate the signal.
--
-- The sample points are linear interpolated.
integrate :: TrackPos -> Signal -> Signal
integrate srate = map_signal_accum go final 0
    where
    go accum x0 y0 x1 y1 =
        integrate_segment (pos_to_val srate) accum x0 y0 x1 y1
    -- Pretend like the last sample extends to the right forever.  This is
    -- grody but seems to be inevitable with a variable sampling rate scheme.
    -- TODO Actually, I only need to do this because the tempo signal doesn't
    -- say how long the block is.  If the tempo track put a sample at the end
    -- of the block I think I wouldn't need this...
    big = pos_to_val max_track_pos
    final ((x, y), accum) = [(x, accum), (big, accum + y * (big-x))]

integrate_segment :: Val -> Val -> Val -> Val -> Val -> Val
    -> (Val, [(Val, Val)])
integrate_segment srate accum x0 y0 x1 y1
    | x0 >= x1 = (accum, [])
        -- A line with slope 0 can be integrated without sampling.
        -- The final point is left for the beginning of the next segment.
    | y0 == y1 = (accum + (x1-x0)*y0, [(x0, accum)])
    | otherwise = (y_at x1, [(x, y_at x) | x <- samples])
    where
    samples = takeWhile (<x1) (sample_stream x0 srate)
    -- math is hard let's go shopping
    y_at x = accum + ((x-x0)**2 / (2/slope)) + (y0 * (x-x0))
    slope = (y1-y0) / (x1-x0)

-- | Truncate a signal.  It's just a view of the old signal, so it
-- doesn't allocate a new signal.
truncate :: TrackPos -> Signal -> Signal
truncate pos (Signal vec) = Signal below
    where (below, _) = V.splitAt (bsearch_on vec fst (pos_to_val pos)) vec

map_val :: (Val -> Val) -> Signal -> Signal
map_val f = vmap (V.map (Arrow.second f))

map_pos :: (TrackPos -> TrackPos) -> Signal -> Signal
map_pos f = vmap (V.map (Arrow.first (pos_to_val . f . val_to_pos)))

-- ** implementation

clip_with :: Val -> (Val -> Val -> Bool) -> Signal -> Signal
clip_with val f = map_signal_accum go final False
    where
    final ((x, y), _) = [(x, if f y val then val else y)]
        -- True if I'm in a clipped segment, False otherwise.
    go True x0 y0 x1 y1
        | f y1 val = (True, [])
        | x == x1 = (False, [(x1, y1)])
        | otherwise = (False, [(x, val), (x1, y1)])
        where x = x_at x0 y0 x1 y1 val
    go False x0 y0 x1 y1
            -- y0==y1 means a flat line started past the limit, like say a zero
            -- signal.
        | f y1 val = if y0 == y1 then (True, [(x0, val), (x1, val)])
            else (True, [(x, val)])
        | otherwise = (False, [(x1, y1)])
        where x = x_at x0 y0 x1 y1 val

-- | Generate sample points from @start@ at @srate@.  This uses multiplication
-- instead of successive addition to avoid loss of precision.
sample_stream :: (Enum a, Num a) => a -> a -> [a]
sample_stream start srate = map ((start+) . (srate*)) [0..]

-- * util

first_to_pos (a, b) = (val_to_pos a, b)
first_to_val (a, b) = (pos_to_val a, b)
vmap f (Signal vec) = Signal (f vec)

-- | Map a function across pairs of samples, threading an additional
-- accumulator through for state.  The function is passed the *previous* sample
-- along with the current one, so it should return samples based on the second
-- sample it receives (the sample "previous" to the first sample will be (0,
-- 0)).  Also, the function returns a list of samples, so this is also like
-- concatMap.
--
-- TODO I should be able to do a faster version of this by working directly
-- with the pointers.
map_signal_accum ::
    (accum -> Val -> Val -> Val -> Val -> (accum, [(Val, Val)]))
    -- ^ Take the previous accum, previous x and y, and current x and y.
    -> (((Val, Val), accum) -> [(Val, Val)]) -> accum -> Signal -> Signal
    -- ^ Optionally reduce the final ((x, y), accum) to samples to append.
map_signal_accum f final accum (Signal vec) =
    Signal (V.pack (DList.toList result))
    where
    (last_accum, _, dlist) = V.foldl' go (accum, (0, 0), DList.empty) vec
    end = if V.null vec then [] else final (V.last vec, last_accum)
    result = dlist `DList.append` DList.fromList end
    go (accum, (x0, y0), lst) (x1, y1) =
        (accum2, (x1, y1), lst `DList.append` DList.fromList samples)
        where (accum2, samples) = f accum x0 y0 x1 y1

map_signal :: (Val -> Val -> Val -> Val -> [(Val, Val)]) -> Signal -> Signal
map_signal f = map_signal_accum go (const []) ()
    where go _ x0 y0 x1 y1 = ((), f x0 y0 x1 y1)

-- | Given a line defined by the two points, find the y at the given x.
y_at :: Val -> Val -> Val -> Val -> Val -> Val
y_at x0 y0 x1 y1 x
    | x == x1 = y1 -- avoid zero length segments
    | otherwise = (y1 - y0) / (x1 - x0) * (x - x0) + y0

-- | Given a line defined by the two points, find the x at the given y.
x_at :: Val -> Val -> Val -> Val -> Val -> Val
x_at x0 y0 x1 y1 y
    | x0 == x1 = x1 -- zero width means vertical, which means it crosses here
    | y0 == y1 = error $ "x_at on flat line " ++ show ((x0, y0), (x1, y1), y)
    | otherwise = (y - y0) / ((y1 - y0) / (x1 - x0)) + x0

pos_to_val :: TrackPos -> Val
pos_to_val = realToFrac
val_to_pos :: Val -> TrackPos
val_to_pos = realToFrac
