{-
Unfortunately there are four ways to describe an interpolation:

A Method is simply the interpolation type by itself: Set, Linear, etc.  It
directly corresponds to the notation on the track, and is used with
TrackSegments, which are convenient to write on the track but not convenient to
use for interpolation, since each segment's beginning point is the last
segment's end point.  There is one Method for each kind of hardcoded
interpolation type supported by the track deriver.

A Segment is how segments are stored in the Signal.  There are only two kinds
of segments: SegLinear, and SegFunction.  Linear segments are preserved because
they can be implemented and drawn efficiently, and SegFunction encompasses all
the other kinds of interpolation.

A Sample is a further simplification of Segment down to only linear line
segments.  The SegFunctions have been sampled out, and this yields a signal that
can be easily integrated, inverted, rendered to a pwl function, or drawn on the
GUI.

The 'sample' function returns [(TrackPos, Val)], and this is the simplest
version of all, effectively consisting of only sets.  This is what you use for
something like MIDI that doesn't even understand linear segments.
-}
module Perform.Signal where
import qualified Control.Arrow as Arrow
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Data
import qualified Util.Seq as Seq

import Ui.Types
import qualified Perform.Timestamp as Timestamp


-- * constants

-- Later, this should be under deriver control.
default_srate = TrackPos 100
default_srate_ts = Timestamp.from_track_pos default_srate

-- | Make a constant signal.
constant :: Val -> Signal
constant val = signal [(TrackPos 0, Set, val, val)]

-- * construction

newtype Signal = Signal (Map.Map TrackPos (TrackPos, Segment))
    deriving (Show)

-- | This is how signal segments are represented on the track.
type TrackSegment = (TrackPos, Method, Val, Val)
type Val = Double

signal :: [TrackSegment] -> Signal
signal track_segs = Signal . Map.fromAscList . filter (not.zero_width) $
    segs ++ [last_seg]
    -- The initial (TrackPos 0, 0) will create a leading 0 segment that will
    -- be replaced if there's already a segment at 0.
    where
    ((last_pos, last_val), segs) = List.mapAccumL go (TrackPos 0, 0) track_segs
    -- The convention is that Signals stay constant after the last segment, so
    -- attach a short flat segment to zero the slope.  The various
    -- interpolation functions will extend it indefinitely.
    last_seg = (last_pos, (last_pos + TrackPos 1, SegLinear last_val last_val))
    go (pos0, val0) (pos1, meth, to_val1, from_val1) =
            ((pos1, from_val1), (pos0, (pos1, seg)))
        where
        seg = case meth of
            Set -> SegLinear val0 val0
            Linear -> SegLinear val0 to_val1
            Exp n -> SegFunction ("exp " ++ show n)
                (exp_function n val0 to_val1)
    zero_width (p0, (p1, _)) = p0 == p1

exp_function n val0 val1 amount
    | amount >= 1 = val1
    | otherwise = val0 + amount**exp * (val1 - val0)
    where exp = if n >= 0 then n else (1 / abs n)

data Method = Set | Linear
    -- | Approach the point with an exponential curve.  If the exponent is
    -- positive, the value will be pos**n.  If it's negative, it will be
    -- pos**(1/n).
    | Exp Double
    deriving (Show, Eq)

data Segment = SegLinear Val Val
    -- | A function has a name for reference, and a number between 0 and 1 to
    -- the signal's Val at that point.
    | SegFunction String (Double -> Val)
instance Show Segment where
    show (SegLinear v0 v1) = "SegLinear " ++ show v0 ++ " " ++ show v1
    show (SegFunction name _) = "SegFunction " ++ show name ++ " <func>"

-- | The second TrackPos is redundant because it should always be the same
-- as the first TrackPos of the next Sample, but it's convenient to be able
-- to treat Samples alone.  TODO: or is it?  remove it if not
data Sample = Sample TrackPos Val TrackPos Val deriving (Eq, Show)

-- * sample

-- | Reduce signal to (TrackPos, Val).
sample :: TrackPos -> Signal -> TrackPos -> [(TrackPos, Val)]
sample srate (Signal smap) start = Seq.drop_dups dup_val $
    concatMap (sample_seg srate) assocs
    where assocs = Map.toAscList (snd (Util.Data.split_map start smap))

-- | Like 'sample', but return the samples at timestamps.
sample_timestamp :: Timestamp.Timestamp -> Signal -> Timestamp.Timestamp
    -> [(Timestamp.Timestamp, Val)]
sample_timestamp srate sig start =
    map (Arrow.first Timestamp.from_track_pos) $
        sample (Timestamp.to_track_pos srate) sig (Timestamp.to_track_pos start)

-- | Reduce signal to samples, which are straight line segments.
linear_sample :: TrackPos -> Signal -> [Sample]
linear_sample srate (Signal smap) =
    concatMap (seg_to_sample srate) (Map.assocs smap)


-- Segments that line up with each other can make redundant samples.
dup_val (_pos0, val0) (_pos1, val1) = val0 == val1

sample_seg :: TrackPos -> (TrackPos, (TrackPos, Segment)) -> [(TrackPos, Val)]
sample_seg srate (pos0, (pos1, seg)) =
    zip smps (map (interpolate seg pos0 pos1) smps)
    where smps = takeWhile (<pos1) [pos0, pos0 + srate..]


seg_to_sample srate (pos0, (pos1, seg)) = case seg of
    SegFunction _ _ ->
        let ps = takeWhile (<pos1) [pos0, pos0 + srate..]
            -- TODO It would be more accurate to make this linear with the
            -- sample in the middle.
            mksample p0 p1 val = Sample p0 val p1 val
        in List.zipWith3 mksample
            -- The drop loses the last sample, but that's ok because it would
            -- overlap with the next one anyway.
            ps (drop 1 ps) (map (interpolate seg pos0 pos1) ps)
    SegLinear val0 val1 -> [Sample pos0 val0 pos1 val1]

-- * at

-- | Get the value of a signal at the given timestamp.
at :: Signal -> TrackPos -> Val
at (Signal smap) pos = interpolate seg pos0 pos1 pos
    where
    (pos0, (pos1, seg)) = maybe (TrackPos 0, (TrackPos 0, SegLinear 0 0)) id $
        lookup_at_below pos smap

at_timestamp :: Signal -> Timestamp.Timestamp -> Val
at_timestamp sig ts = at sig (Timestamp.to_track_pos ts)

-- | Return an element at @k@ or one before it, or Nothing.
lookup_at_below k fm = case at of
    Nothing -> Util.Data.find_max below
    Just a -> Just (k, a)
    where
    (below, at, _) = Map.splitLookup k fm

interpolate :: Segment -> TrackPos -> TrackPos -> TrackPos -> Val
interpolate seg start end pos = case seg of
    SegLinear val0 val1
        | pos >= end -> val1
        | otherwise -> val0 + amount * (val1 - val0)
    SegFunction _ f -> f amount
    where
    amount = fromIntegral (pos - start) / fromIntegral (end - start)


-- * transformations

-- TODO implement
equal :: TrackPos -> TrackPos -> Signal -> Signal -> Bool
equal start end sig0 sig1 = False

-- ** inverse

inverse :: TrackPos -> Signal -> [Val] -> [TrackPos]
inverse srate sig vals = go (linear_sample srate sig) vals
    where
    go _ [] = []
    go [] (v:_) = error $ "inverse: samples ended before finding " ++ show v
    go samples@(Sample p0 v0 p1 v1 : rest_samples) vals@(v:vs)
        -- Since I assume that both samples and vals are monotonically
        -- nondecreasing, this means either I passed v with a jump, or v
        -- decreased.
        | v0 >= v = p0 : go samples vs
        | v1 >= v = round (x_at (fromIntegral p0, v0) (fromIntegral p1, v1) v)
            : go samples vs
        | otherwise = go rest_samples vals

-- ** integrate

integrate :: TrackPos -> Signal -> [TrackPos] -> [Val]
integrate srate sig pos = go 0 (linear_sample srate sig) pos
    where
    go _ _ [] = []
    go accum [] pos = map (const accum) pos -- null signal is considered 0
    go accum samples@(Sample p0 v0 p1 v1 : rest_samples) pos@(p:rest_ps)
        | null rest_samples = accum_to p : go accum samples rest_ps
        | p < p1 = accum_to p : go accum samples rest_ps
        | otherwise = go (accum_to p1) rest_samples pos
        where
        accum_to xpos = accum + integrate_linear
            (fromIntegral p0, v0) (fromIntegral p1, v1) (fromIntegral xpos)

-- This is way too complicated.
integrate_linear (x0, y0) (x1, y1) x
    | y0 == y1 = (x-x0) * y0 -- Shortcut for constant segments.
        -- If it crosses y=0, integrate both halves seperately.
    | y0 < 0 && y1 > 0 || y0 > 0 && y1 < 0 =
        let x_cross = x_at (x0, y0) (x1, y1) 0
        in integrate_linear (x0, y0) (x_cross, 0) (min x_cross x)
            + if x <= x_cross then 0
                else integrate_linear (x_cross, 0) (x1, y1) x
    | otherwise = rect_h*w + tri_h*w / 2
    where
    tri_h = if positive then abs (y-y0) else -abs (y-y0)
    rect_h = if positive && y0 <= y1 || not positive && y0 >= y1
        then y - tri_h else y
    y = y_at (x0, y0) (x1, y1) x
    w = x - x0
    -- If it crosses zero, the top level guard should have caught it.
    positive = y0 > 0 && y1 > 0

-- | Given a line defined by the two points, find the y at the given x.
y_at :: (Ord a, Fractional a) => (a, a) -> (a, a) -> a -> a
y_at (x0, y0) (x1, y1) x = (y1 - y0) / (x1 - x0) * (x - x0) + y0

-- | Given a line defined by the two points, find the x at the given y.
x_at :: (Ord a, Fractional a) => (a, a) -> (a, a) -> a -> a
x_at (x0, y0) (x1, y1) y = (y - y0) / ((y1-y0) / (x1-x0)) + x0
