{-# OPTIONS_GHC -XFlexibleInstances #-}
{- | This module implements signals as sparse arrays of Val->Val.  The
points are interpolated linearly, so the signal array represents a series of
straight line segments.

By convention, the final segment of a signal is interpreted as extending
infinitely to the right, at a 0 slope.


operations:
- map a function pointwise (e.g. (/1) or (+n)... this is actually composition
- clip (I can't map (min x) because I want to split a line into line + flat
segment
- compose, implemented incrementally for efficiency (also could theoretically
gc the head?)
- integrate
- invert


- sample (for midi performance)
- equal - compare ranges for midi performance


Since I'll wind up computing everything anyway, strict may not be such
a problem.  Also, control signals are sparse compared to audio:

srate of 1000 * (Double, Double) = 128 = 128000 = 125k/sec = 45mb hour, hmm
that is a lot

Laziness actually is usable because segment rendering is a red herring, it
only happens to the track signals.  Meanwhile there are intermediate signals.
I actually don't have many of those, just tempo.  So far.

And the important thing is that if I use the same interface, I can swap out
the implementation for a lazy one later.


data Signal = SignalFunction (TrackPos -> Val)
    | Vector TrackPos Val
    -- Compose Signal Signal ?

data Vector a b = Vector (SVector a) (SVector b)

The problem with SignalFunction is that I lose the sample positions, so
(compose (+1) sig) wouldn't work as well during midi performance.  So
how about just using map.

do some speed tests for large vectors, i.e. integrate a large signal

-}

module Perform.Signal where
import qualified Control.Arrow as Arrow
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.StorableVector as V
import qualified Foreign.Storable as Storable

-- import qualified Util.Data

import Ui.Types
import qualified Ui.Track as Track

import qualified Perform.Timestamp as Timestamp

-- * construction / deconstruction

data Signal =
    -- | The samples in this vector are spaced irregularly, and expected to
    -- be interpolated linearly.
    --
    -- This is a strict vector for now, eventually I may want to switch this
    -- to a lazy one.
    SignalVector (V.Vector (Double, Double))

instance Storable.Storable (Double, Double) where
    sizeOf _ = Storable.sizeOf (undefined :: Double) * 2
    alignment _ = 8
    poke cp (a, b) = Storable.pokeByteOff cp 0 a >> Storable.pokeByteOff cp 8 b
    peek cp = do
        a <- Storable.peekByteOff cp 0 :: IO Double
        b <- Storable.peekByteOff cp 8 :: IO Double
        return ((realToFrac) a, (realToFrac) b)

instance Show Signal where
    show (SignalVector vec) = "Signal <" ++ show (V.unpack vec) ++ ">"

type Val = Double
type Sample = (TrackPos, Val)

signal :: [(TrackPos, Val)] -> Signal
signal vals =
    SignalVector (V.pack (map (\(a, b) -> (pos_to_val a, b)) vals))

constant :: Val -> Signal
constant n = signal [(0, n), (max_track_pos, n)]

unpack :: Signal -> [(TrackPos, Val)]
unpack (SignalVector vec) = map (Arrow.first val_to_pos) (V.unpack vec)

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
    SignalVector $ V.pack (map first_to_val (concat pairs ++ [last_sample]))
    where
    ((_, last_val), pairs) = List.mapAccumL go (TrackPos 0, 0) segs
    last_sample = (max_track_pos, last_val)
    go (pos0, val0) (pos1, meth, val1) = ((pos1, val1), samples)
        where samples = sample_track_seg srate pos0 val0 pos1 val1 meth

sample_track_seg :: TrackPos -> TrackPos -> Val -> TrackPos -> Val -> Method
    -> [Sample]
sample_track_seg srate pos0 val0 pos1 val1 meth = case meth of
    Set | val0 == val1 -> [(pos1, val1)]
        | otherwise -> [(pos1, val0), (pos1, val1)]
    Linear -> [(pos0, val0), (pos1, val1)]
    Exp n -> sample_function (exp_function n val0 val1) srate pos0 pos1

sample_function :: (Double -> Double) -> TrackPos -> TrackPos -> TrackPos
    -> [Sample]
sample_function f srate start end = zip samples (map f points)
    where
    samples = takeWhile (<end) [start, start + srate ..]
    points = map (\p -> realToFrac ((p-start) / (end-start))) samples

-- *** interpolation functions

exp_function n val0 val1 amount
    | amount >= 1 = val1
    | otherwise = val0 + amount**exp * (val1 - val0)
    where exp = if n >= 0 then n else (1 / abs n)


-- * constants

-- | This is used to extend the last segment "forever" and to make endless
-- constant slope signals.  A little icky, but seems simpler than special
-- logic.
max_track_pos :: TrackPos
max_track_pos = TrackPos (2^52 - 1) -- 52 bits of mantissa only should be enough

-- Later, this should be under deriver control.
default_srate = TrackPos 0.05
default_srate_ts = Timestamp.from_track_pos default_srate

-- * equal

-- | Are the given signals equal within the given range?
equal :: TrackPos -> TrackPos -> TrackPos -> Signal -> Signal -> Bool
equal srate start end sig0 sig1 = s0 == s1
    where
    s0 = takeWhile ((<end) . fst) $ sample srate start sig0
    s1 = takeWhile ((<end) . fst) $ sample srate start sig1

-- * access

at :: TrackPos -> Signal -> Val
at pos sig = interpolate_linear pos0 val0 pos1 val1 pos
    where ((pos0, val0), (pos1, val1)) = find_samples pos sig

at_timestamp :: Timestamp.Timestamp -> Signal -> Val
at_timestamp ts sig = at (Timestamp.to_track_pos ts) sig

-- | Sample the signal to generate samples.  This is for MIDI, which doesn't
-- have linear segments, so they have to be interpolated to flat samples.
--
-- This won't emit consecutive samples with the same value.  This might
-- cause problems if there some MIDI instrument treats them as significant,
-- but I'll worry about that if I find one.
sample :: TrackPos -> TrackPos -> Signal -> [Sample]
sample srate start sig@(SignalVector vec) =
    -- The interpolation doesn't include the sample coming from, so it will
    -- be missing the first one, and always interpolates the entire segment,
    -- so it'll have extra stuff on the beginning.
    (start, at start sig) : dropWhile ((<=start) . fst) samples
    where
    samples = map first_to_pos $ concat $ snd $
        List.mapAccumL go (0, 0) (V.unpack (V.drop start_i vec))
    start_i = max 0 (find_above (pos_to_val start) vec - 1)
    go (x0, y0) (x1, y1) = ((x1, y1), samples)
        where
        samples
            | y0 == y1 = []
            | otherwise = interpolate_samples (pos_to_val srate) x0 y0 x1 y1

sample_timestamp :: Timestamp.Timestamp -> Timestamp.Timestamp -> Signal
    -> [(Timestamp.Timestamp, Val)]
sample_timestamp srate start sig = map (Arrow.first Timestamp.from_track_pos) $
    sample (Timestamp.to_track_pos srate) (Timestamp.to_track_pos start) sig


interpolate_samples :: Val -> Val -> Val -> Val -> Val -> [(Val, Val)]
interpolate_samples srate x0 y0 x1 y1 =
    zip xs (map (y_at x0 y0 x1 y1) xs)
        -- Skip the first sample, because that will have been set by the final
        -- sample of the previous segment.
    where xs = range (x0+srate) x1 srate

-- | Like enumFromTo except always include the final value.
range start end step
    | start >= end = [end]
    | otherwise = start : range (start+step) end step

-- ** implementation

-- Before the first sample: (zero, first)
-- at the first until the second: (first, second)
-- at the last until whenever: (last, extend last in a straight line)
find_samples :: TrackPos -> Signal -> ((TrackPos, Val), (TrackPos, Val))
find_samples pos (SignalVector vec)
    | len == 0 = (zero, (TrackPos 1, 0))
    | otherwise = (ix (i-1), ix i)
    where
    (!) = V.index
    len = V.length vec
    ix i
        | i < 0 = zero
        -- The convention is that Signals stay constant after the last segment,
        -- so attach a short flat segment to zero the slope.  The various
        -- interpolation functions will extend it indefinitely.
        | i >= len = Arrow.first (val_to_pos . (+1)) (vec ! (len-1))
        | otherwise = first_to_pos (vec!i)
    zero = (TrackPos 0, 0)
    i = find_above (pos_to_val pos) vec

-- | Find the index of the first element > the given pos.  It's the length of
-- the vector if there is no such element.
find_above :: Val -> V.Vector (Val, Val) -> Int
find_above pos vec = Maybe.fromMaybe len $
        List.find (\n -> fst (vec `V.index` n) > pos) [i..len-1]
    where
    len = V.length vec
    i = bsearch_on vec fst pos

-- | Find the index of the first element >= the key of the given element.
bsearch_on vec key v = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | v <= key (vec `V.index` mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2

interpolate_linear x0 y0 x1 y1 x = y0 + amount * (y1-y0)
    where amount = realToFrac $ (x-x0) / (x1-x0)

-- * functions

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
inverse_at (SignalVector vec) ts
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

-- | Integrate the signal.
integrate :: TrackPos -> Signal -> Signal
integrate srate = map_signal_accum go 0
    where
    go accum x0 y0 x1 y1 =
        integrate_segment (pos_to_val srate) accum x0 y0 x1 y1

-- | Clip signal to never go over the given val.  This is different from
-- mapping 'max' because it will split segments at the point they go out of
-- bounds and insert a flat segment.
clip_max :: Val -> Signal -> Signal
clip_max max_val = clip_with max_val (>)

clip_min :: Val -> Signal -> Signal
clip_min min_val = clip_with min_val (<)

map_samples :: (Val -> Val) -> Signal -> Signal
map_samples f = vmap (V.map (\(x, y) -> (x, f y)))

-- ** implementation

clip_with val f = map_signal_accum go False
    where
    go True x0 y0 x1 y1
        | f y1 val = (True, [])
        | x == x1 = (False, [(x1, y1)])
        | otherwise = (False, [(x, val), (x1, y1)])
            where x = x_at x0 y0 x1 y1 val
    go False x0 y0 x1 y1
        | f y1 val = (True, [(x, val)])
        | otherwise = (False, [(x1, y1)])
            where x = x_at x0 y0 x1 y1 val

integrate_segment :: Val -> Val -> Val -> Val -> Val -> Val
    -> (Val, [(Val, Val)])
integrate_segment srate accum x0 y0 x1 y1
    | x0 == x1 = (accum, [])
        -- Line with slope y0, take a shortcut.
    | y0 == y1 =
        let x = x1 - srate
            y = accum + x * y0
        in (y, [(x0, accum), (x, y)])
    | otherwise = List.mapAccumL go accum samples
    where
    samples = takeWhile (<x1) [x0, x0 + srate ..]
    go accum x = let val = accum + y_at x0 y0 x1 y1 x * srate in (val, (x, val))


-- * util

first_to_pos (a, b) = (val_to_pos a, b)
first_to_val (a, b) = (pos_to_val a, b)
vmap f (SignalVector vec) = SignalVector (f vec)

map_signal_accum :: (accum -> Val -> Val -> Val -> Val -> (accum, [(Val, Val)]))
    -> accum -> Signal -> Signal
map_signal_accum f accum (SignalVector vec) =
    SignalVector (V.pack (DList.toList result))
    where
    result = (\(_, _, x) -> x) $ V.foldl' go (accum, (0, 0), DList.empty) vec
    go (accum, (x0, y0), lst) (x1, y1) =
        (accum2, (x1, y1), lst `DList.append` DList.fromList samples)
        where (accum2, samples) = f accum x0 y0 x1 y1

map_signal :: (Val -> Val -> Val -> Val -> [(Val, Val)]) -> Signal -> Signal
map_signal f = map_signal_accum go False
    where go _ x0 y0 x1 y1 = (False, f x0 y0 x1 y1)

-- | Given a line defined by the two points, find the y at the given x.
y_at :: Val -> Val -> Val -> Val -> Val -> Val
y_at x0 y0 x1 y1 x
    | x == x1 = y1 -- avoid zero length segments
    | otherwise = (y1 - y0) / (x1 - x0) * (x - x0) + y0

-- | Given a line defined by the two points, find the x at the given y.
x_at :: Val -> Val -> Val -> Val -> Val -> Val
x_at x0 y0 x1 y1 y
    | x0 == x1 = x1 -- zero width means vertical, which means it crosses here
    | y0 == y1 = error $ "x_at on flat line " ++ show y0
    | otherwise = (y - y0) / ((y1 - y0) / (x1 - x0)) + x0

pos_to_val :: TrackPos -> Val
pos_to_val = realToFrac
val_to_pos :: Val -> TrackPos
val_to_pos = realToFrac

{-
lookup_around_loose def k fm = case lookup_around k fm of
    (Just kv0, Just kv1) -> (kv0, kv1)
    (Nothing, Nothing) -> (def, def)
    (Nothing, Just kv1) -> (def, kv1)
    (Just kv0@(k0, v0), Nothing) ->
        (Maybe.fromMaybe def (lookup_before k0 fm), kv0)

lookup_before :: (Ord k) => k -> Map.Map k a -> Maybe (k, a)
lookup_before k fm = Util.Data.find_max (fst (Map.split k fm))

lookup_around k fm = case at of
        Nothing -> (Util.Data.find_max pre, Util.Data.find_min post)
        Just v -> (Just (k, v), Util.Data.find_min post)
    where (pre, at, post) = Map.splitLookup k fm
-}
