-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE EmptyDataDecls #-}
{- | Sample values are doubles, which means each point in the signal is 8*2
    bytes.  The double resolution is overkill for the value, but float would
    be too small for time given the time stretching.

    TODO split this into Float and Double versions since only Warp really
    needs Double.  Or does Warp really need Double?
-}
module Perform.Signal2 (
    -- * types
    Signal, Sample(..)
    , X, Y, x_to_y, y_to_x, y_to_score, y_to_nn, nn_to_y
    , Tempo, Warp, Control, NoteNumber, Display

    -- * construct / destruct
    , from_pairs, from_segments
    , to_samples, to_pairs, to_segments
    , constant, constant_val
    , unfoldr
    , coerce
    , with_ptr

    -- * query
    -- , length
    , null
    , at, segment_at
    -- , head, last
    , minimum, maximum

    -- * transform

    , drop_after, drop_before
    , clip_after, clip_before
    -- , drop, drop_while, within
    -- , drop_at_after, drop_after, drop_before, drop_before_strict
    -- , drop_before_at
    , shift

    , invert, sig_add, sig_subtract, sig_multiply, sig_scale
    , scale, scale_invert

    -- ** scalar transformation
    , scalar_max, scalar_min
    -- , clip_bounds
    , scalar_add, scalar_subtract, scalar_multiply, scalar_divide
    , map_x, map_y, map_err

    -- * special functions
    , integrate
    , flat_duration
    , pitches_share
) where
import qualified Prelude
import Prelude hiding (concat, head, last, length, maximum, minimum, null, drop)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Vector.Storable as Vector.Storable
import qualified Foreign

import qualified Util.Num as Num
import qualified Util.Segment as Segment
import Util.Segment (X, Sample(..))
import qualified Util.Serialize as Serialize
import qualified Util.TimeVector as TimeVector

import qualified Midi.Midi as Midi
import qualified Ui.ScoreTime as ScoreTime
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Global
import Types


-- * types

-- | A Signal is a 'Segment.Signal' of 'Y' values, which are just Doubles.  It
-- takes a phantom type parameter to make the signal's intended uses a little
-- clearer.  There are type aliases for the various flavors of signal below,
-- but it really is just documentation and anyone who wants to operate on
-- a generic signal can take a @Signal kind@.
newtype Signal kind = Signal Segment.NumSignal
    deriving (Show, Pretty, Eq, DeepSeq.NFData, Serialize.Serialize)

_signal :: Signal kind -> Segment.NumSignal
_signal (Signal sig) = sig

modify :: (Segment.NumSignal -> Segment.NumSignal) -> Signal kind -> Signal kind
modify f = Signal . f . _signal

type Y = Double

instance Monoid (Signal kind) where
    mempty = Signal Segment.empty
    mappend s1 s2
        | null s1 = s2
        | null s2 = s1
        | otherwise = Signal $ Segment.concat [_signal s1, _signal s2]
    mconcat = Signal . Segment.concat . map _signal

-- | This is the type of performer-interpreted controls that go into the
-- event's control map.
type Control = Signal ControlSig
data ControlSig

-- | A tempo is a normal Control signal, except that instead of going into the
-- control map, it gets turned into a Warp and goes into the warp map.
type Tempo = Signal TempoSig
data TempoSig

-- | A tempo warp maps score time to real time.  Of course the type is still
-- (ScoreTime, Y), so functions that process Warps have to convert.
type Warp = Signal WarpSig
data WarpSig

-- | This is the type of pitch signals used by the performer, after the scale
-- has been factored out.
type NoteNumber = Signal NoteNumberSig
data NoteNumberSig

-- | This is the type of signals which are sent to the UI for display.
type Display = Signal DisplaySig
data DisplaySig

x_to_y :: X -> Y
x_to_y = RealTime.to_seconds

y_to_x :: Y -> X
y_to_x = RealTime.seconds

-- | Some control signals may be interpreted as score time.
y_to_score :: Y -> ScoreTime
y_to_score = ScoreTime.double

y_to_nn :: Y -> Pitch.NoteNumber
y_to_nn = Pitch.NoteNumber

nn_to_y :: Pitch.NoteNumber -> Y
nn_to_y (Pitch.NoteNumber nn) = nn

-- * construct / destruct

from_pairs :: [(X, Y)] -> Signal kind
from_pairs = Signal . Segment.from_pairs

from_segments :: [Segment.Segment Y] -> Signal kind
from_segments = Signal . Segment.from_segments

to_samples :: Signal kind -> [Sample Y]
to_samples = Segment.to_samples . _signal

to_pairs :: Signal kind -> [(X, Y)]
to_pairs = Segment.to_pairs . _signal

to_segments :: Signal kind -> [Segment.Segment Y]
to_segments = Segment.to_segments . _signal

constant :: Y -> Signal kind
constant = Signal . Segment.constant

-- | Just if the signal is constant.
constant_val :: Signal kind -> Maybe Y
constant_val = Segment.constant_val_num . _signal

unfoldr :: (state -> Maybe ((X, Y), state)) -> state -> Signal kind
unfoldr gen state = Signal $ Segment.unfoldr gen state

-- | Sometimes signal types need to be converted.
coerce :: Signal kind1 -> Signal kind2
coerce (Signal vec) = Signal vec

-- | 'Segment.with_ptr'.
with_ptr :: Display -> (X -> Foreign.Ptr (Sample Y) -> Int -> IO a) -> IO a
with_ptr sig = Segment.with_ptr (_signal sig)

-- * query

-- length :: Signal kind -> Int
-- length = TimeVector.length . sig_vec

null :: Signal kind -> Bool
null = Segment.null . _signal

at :: X -> Signal kind -> Y
at x = fromMaybe 0 . Segment.at Segment.num_interpolate x . _signal

segment_at :: X -> Signal kind -> Maybe (Segment.Segment Y)
segment_at x = Segment.segment_at x . _signal

-- * transform

drop_after, drop_before :: X -> Signal kind -> Signal kind
drop_after x = modify $ Segment.drop_after x
drop_before x = modify $ Segment.drop_before x

clip_after, clip_before :: X -> Signal kind -> Signal kind
clip_after x = modify $ Segment.clip_after Segment.num_interpolate x
clip_before x = modify $ Segment.clip_before Segment.num_interpolate x

-- TODO used by?
{-
drop :: Int -> Signal kind -> Signal kind
drop = modify . TimeVector.drop

-- slice in Midi.Perform
drop_while :: (Sample Y -> Bool) -> Signal kind -> Signal kind
drop_while = modify . Vector.dropWhile

-- Midi.Perform.controls_equal
within :: X -> X -> Signal kind -> Signal kind
within start end = modify $ TimeVector.within start end

-- Block.trim_controls, trill xcut, Derive.Control.trim_signal
drop_at_after :: X -> Signal kind -> Signal kind
drop_at_after = modify . TimeVector.drop_at_after

-- trim_signal
drop_after :: X -> Signal kind -> Signal kind
drop_after = modify . TimeVector.drop_after

drop_before :: X -> Signal kind -> Signal kind
drop_before = modify . TimeVector.drop_before

drop_before_strict :: X -> Signal kind -> Signal kind
drop_before_strict = modify . TimeVector.drop_before_strict

drop_before_at :: X -> Signal kind -> Signal kind
drop_before_at = modify . TimeVector.drop_before_at
-}

shift :: X -> Signal kind -> Signal kind
shift x = modify (Segment.shift x)

invert :: Signal kind -> Signal kind
invert = modify Segment.invert

-- prepend :: Signal kind -> Signal kind -> Signal kind
-- prepend s1 s2 = Signal $ TimeVector.prepend (sig_vec s1) (sig_vec s2)

sig_add, sig_multiply :: Control -> Control -> Control
sig_add = linear_operator (Just 0) (+)
sig_multiply = linear_operator (Just 1) (*)

sig_subtract :: Control -> Control -> Control
sig_subtract sig1 sig2
    | Just v <- constant_val sig2, v == 0 = sig1
    | otherwise = linear_operator Nothing (-) sig1 sig2

-- TODO I think this is linear?
sig_scale :: Control -> Control -> Control
sig_scale = linear_operator (Just 1) scale

scale :: Y -> Y -> Y
scale x v
    | v >= 0 = Num.scale x 1 v
    | otherwise = Num.scale 0 x (v + 1)

scale_invert :: Y -> Y -> Y
scale_invert old new
    | new >= old = Num.normalize old 1 new
    | otherwise = Num.normalize 0 old new - 1

linear_operator :: Maybe Y -- ^ If an identity value is given, I can avoid
    -- copying the whole signal if the other one is a constant identity.
    -> (Y -> Y -> Y) -> Signal kind -> Signal kind -> Signal kind
linear_operator (Just identity) _ sig1 sig2
    | Just v <- constant_val sig1, v == identity = sig2
    | Just v <- constant_val sig2, v == identity = sig1
linear_operator _ op sig1 sig2 =
    Signal $ Segment.linear_operator op (_signal sig1) (_signal sig2)

-- ** scalar transformation

scalar_add, scalar_subtract, scalar_multiply, scalar_divide ::
    Y -> Signal kind -> Signal kind
scalar_add n = map_y (+n)
scalar_subtract n = map_y (subtract n)
scalar_multiply n = map_y (*n)
scalar_divide n = map_y (/n)

-- | Clip signal to never go above or below the given value.
scalar_max, scalar_min :: Y -> Signal kind -> Signal kind
scalar_max val sig
    | Just y <- minimum sig, y >= val = sig
    | otherwise = modify (Segment.transform_samples go) sig
    where
    go [] = []
    go [Sample x y] = [Sample x (max val y)]
    go (s1@(Sample x1 y1) : s2@(Sample x2 y2 : sn)) =
        case TimeVector.x_at x1 y1 x2 y2 val of
            Nothing
                | y1 < val -> Sample x1 val : go (Sample x2 val : sn)
                | otherwise -> s1 : go s2
            Just x_val
                | y1 < val -> go (Sample x_val val : s2)
                | otherwise -> s1 : go (Sample x_val val : sn)
scalar_min val sig
    | Just y <- maximum sig, y <= val = sig
    | otherwise = modify (Segment.transform_samples go) sig
    where
    go [] = []
    go [Sample x y] = [Sample x (min val y)]
    go (s1@(Sample x1 y1) : s2@(Sample x2 y2 : sn)) =
        case TimeVector.x_at x1 y1 x2 y2 val of
            Nothing
                | y1 > val -> Sample x1 val : go (Sample x2 val : sn)
                | otherwise -> s1 : go s2
            Just x_val
                | y1 > val -> go (Sample x_val val : s2)
                | otherwise -> s1 : go (Sample x_val val : sn)

minimum, maximum :: Signal kind -> Maybe Y
minimum = Segment.minimum . _signal
maximum = Segment.maximum . _signal

{-
-- | Clip the signal's Y values to lie between (0, 1), inclusive.  Return the
-- half-open ranges during which the Y was out of range, if any.
clip_bounds :: Y -> Y -> Signal kind -> (Signal kind, [(X, X)])
clip_bounds low high sig = (clipped, reverse out_of_range)
    where
    clipped = if Prelude.null out_of_range then sig
        else scalar_min high $ scalar_max low sig
        -- else map_y (Num.clamp low high) sig
    out_of_range = []

    -- (ranges, in_clip) = TimeVector.foldl' go ([], Nothing) (sig_vec sig)
    -- out_of_range = case (in_clip, last sig) of
    --     (Just start, Just (end, _)) -> (start, end) : ranges
    --     _ -> ranges
    -- go state@(accum, Nothing) (Sample x y)
    --     | y < low || y > high = (accum, Just x)
    --     | otherwise = state
    -- go state@(accum, Just start) (Sample x y)
    --     | y < low || y > high = state
    --     | otherwise = ((start, x) : accum, Nothing)

-- | Clip the signal's Y values to lie between (0, 1), inclusive.  Return the
-- half-open ranges during which the Y was out of range, if any.
clip_bounds :: Y -> Y -> Signal kind -> (Signal kind, [(X, X)])
clip_bounds low high sig = (clipped, reverse out_of_range)
    where
    clipped = if Prelude.null out_of_range then sig
        else map_y (Num.clamp low high) sig
    (ranges, in_clip) = TimeVector.foldl' go ([], Nothing) (sig_vec sig)
    out_of_range = case (in_clip, last sig) of
        (Just start, Just (end, _)) -> (start, end) : ranges
        _ -> ranges
    go state@(accum, Nothing) (Sample x y)
        | y < low || y > high = (accum, Just x)
        | otherwise = state
    go state@(accum, Just start) (Sample x y)
        | y < low || y > high = state
        | otherwise = ((start, x) : accum, Nothing)
-}

map_x :: (X -> X) -> Signal kind -> Signal kind
map_x = modify . Segment.map_x

map_y :: (Y -> Y) -> Signal kind -> Signal kind
map_y = modify . Segment.map_y

map_err :: (Sample Y -> Either err (Sample Y)) -> Signal kind
    -> (Signal kind, [err])
map_err f = first Signal . Segment.map_err f . _signal

-- * special functions

-- | Integrate the signal.
--
-- Since the output will have more samples than the input, this needs
-- a sampling rate.  The sampling rate determines the resolution of the tempo
-- track.  So it can probably be fairly low resolution before having
-- a noticeable impact.
integrate :: Tempo -> Warp
integrate = Signal . Segment.integrate tempo_srate . _signal

tempo_srate :: X
tempo_srate = RealTime.seconds 0.1


-- | Total duration of horizontal segments in the warp signal.  These are
-- the places where 'Warp.compose_hybrid' will emit a 1\/1 line.
flat_duration :: Warp -> ScoreTime
flat_duration =
    RealTime.to_score . fst . Vector.Storable.foldl' go (0, Segment.Sample 0 0)
        . Segment.to_vector . _signal
    where
    go (!acc, Segment.Sample x0 y0) sample@(Segment.Sample x y)
        | y == y0 = (acc + (x - x0), sample)
        | otherwise = (acc, sample)

-- ** pitches_share

-- | Can the pitch signals share a channel within the given range?
--
-- Pitch is complicated.  Like other controls, if the pitch curves are
-- different they may not share a channel.  However, if the pitch curves
-- are integral transpositions of each other, and the transposition is not
-- 0, they should share.  Unless the overlap occurs during the decay of one or
-- both notes, at which point 0 transposition is ok.
--
-- This is actually a MIDI notion, so it should normally go in Perform.Midi,
-- but it fusses around with signal internals for efficiency.
-- TODO move to Perform.Midi
--
-- This function takes 'TimeVector.Unboxed's instead of Signals because it
-- operates on low level samples, not linear segments.  It will be confused by
-- multiple samples at the same time, so don't do that.
pitches_share :: Bool -> X -> X
    -> Midi.Key -> TimeVector.Unboxed -> Midi.Key -> TimeVector.Unboxed -> Bool
pitches_share in_decay start end initial1 sig1 initial2 sig2
    | not in_decay && initial1 == initial2 = False
    | otherwise = pitch_eq (sig1 ! start) (sig2 ! start)
        && pitch_eq (sig1 ! end) (sig2 ! end)
        && signals_share pitch_eq start in1 in2
    where
    in1 = TimeVector.within start end sig1
    in2 = TimeVector.within start end sig2
    pitch_eq = nns_share initial1 initial2
    (!) sig x = fromMaybe 0 $ TimeVector.at x sig

-- | I need to sample points from start to end, including the start and the
-- end.  Unfortunately it's not as simple as it seems it should be, especially
-- since this function is a hotspot and must be efficient.
--
-- Segment.within may return samples before start to get the proper value so
-- I ignore samples before the start.  Start itself is tested explicitly above.
{-# INLINE signals_share #-}
signals_share :: (Y -> Y -> Bool) -> X -> TimeVector.Unboxed
    -> TimeVector.Unboxed -> Bool
signals_share eq start vec1 vec2 = go 0 0 0 0
    where
    go prev_ay prev_by i1 i2 =
        case TimeVector.resample1 prev_ay prev_by len1 len2 i1 i2 vec1 vec2 of
            Nothing -> True
            Just (x, ay, by, i1, i2) ->
                (x <= start || eq ay by) && go ay by i1 i2
    len1 = TimeVector.length vec1
    len2 = TimeVector.length vec2

nns_share :: Midi.Key -> Midi.Key -> Y -> Y -> Bool
nns_share initial1 initial2 nn1 nn2 =
    floor ((nn1 - Midi.from_key initial1) * 1000)
        == floor ((nn2 - Midi.from_key initial2) * 1000)
