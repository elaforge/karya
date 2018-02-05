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
module Perform.Signal (
    -- * types
    Signal, Sample(..)
    , X, Y, x_to_y, y_to_x, y_to_score, y_to_nn, nn_to_y
    , Tempo, Warp, Control, NoteNumber, Display

    -- * construct / destruct
    , from_sample, from_pairs, from_segments
    , to_samples, to_pairs, to_pairs_unique, to_segments
    , constant, constant_val
    , prepend
    , unfoldr
    , coerce
    , to_piecewise_constant
    , with_ptr

    -- * query
    , null
    , at, at_maybe, segment_at
    , head, last
    , minimum, maximum

    -- * transform
    , drop_after, drop_before
    , clip_after, clip_before
    , shift

    , invert, sig_add, sig_subtract, sig_multiply, sig_scale
    , scale, scale_invert

    -- ** hacks
    , drop_discontinuity_at

    -- ** scalar transformation
    , scalar_max
    , scalar_add, scalar_subtract, scalar_multiply, scalar_divide
    , map_x, map_y, map_y_linear, map_err

    -- * special functions
    , integrate_inverse
    , integrate
    , flat_duration
) where
import qualified Prelude
import Prelude hiding (head, last, maximum, minimum, null, drop)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Vector.Storable as Vector
import qualified Foreign

import qualified Util.Num as Num
import qualified Util.Segment as Segment
import Util.Segment (X, Sample(..))
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Util.TimeVector as TimeVector

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
        | otherwise = mconcat [s1, s2]
    mconcat = Signal . Segment.concat (Just (==)) Segment.num_interpolate
        . map _signal

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

from_sample :: X -> Y -> Signal kind
from_sample x y = from_pairs [(x, y)]

from_pairs :: [(X, Y)] -> Signal kind
from_pairs = Signal . Segment.from_pairs

from_segments :: [Segment.Segment Y] -> Signal kind
from_segments = Signal . Segment.from_segments

to_samples :: Signal kind -> [Sample Y]
to_samples = Segment.to_samples . _signal

to_pairs :: Signal kind -> [(X, Y)]
to_pairs = Seq.drop_dups id . Segment.to_pairs . _signal
    -- Since Segment functions don't have Eq y, they can emit duplicate
    -- samples.  They should be harmless but they clutter tests.

-- | Like 'to_pairs', but filter out explicit discontinuities.  This is because
-- tests were written before they existed, so a lot will break.
-- TODO update the tests
to_pairs_unique :: Signal kind -> [(X, Y)]
to_pairs_unique = Seq.drop_initial_dups fst . to_pairs

to_segments :: Signal kind -> [Segment.Segment Y]
to_segments = Segment.to_segments . _signal

constant :: Y -> Signal kind
constant = Signal . Segment.constant

-- | Just if the signal is constant.
constant_val :: Signal kind -> Maybe Y
constant_val = Segment.constant_val_num . _signal

prepend :: Signal kind -> Signal kind -> Signal kind
prepend sig1 sig2 = Signal $
    Segment.prepend (Just (==)) Segment.num_interpolate
        (_signal sig1) (_signal sig2)

unfoldr :: (state -> Maybe ((X, Y), state)) -> state -> Signal kind
unfoldr gen state = Signal $ Segment.unfoldr gen state

-- | Sometimes signal types need to be converted.
coerce :: Signal kind1 -> Signal kind2
coerce (Signal vec) = Signal vec

to_piecewise_constant :: X -> Signal kind -> TimeVector.Unboxed
to_piecewise_constant srate = Segment.to_piecewise_constant srate . _signal

-- | 'Segment.with_ptr'.
with_ptr :: Display -> (X -> Foreign.Ptr (Sample Y) -> Int -> IO a) -> IO a
with_ptr sig = Segment.with_ptr (_signal sig)

-- * query

null :: Signal kind -> Bool
null = Segment.null . _signal

at :: X -> Signal kind -> Y
at x = fromMaybe 0 . at_maybe x

at_maybe :: X -> Signal kind -> Maybe Y
at_maybe x = Segment.at Segment.num_interpolate x . _signal

segment_at :: X -> Signal kind -> Maybe (Segment.Segment Y)
segment_at x = Segment.segment_at x . _signal

head, last :: Signal kind -> Maybe (X, Y)
head = Segment.head . _signal
last = Segment.last . _signal

-- * transform

drop_after, drop_before :: X -> Signal kind -> Signal kind
drop_after x = modify $ Segment.drop_after x
drop_before x = modify $ Segment.drop_before x

clip_after, clip_before :: X -> Signal kind -> Signal kind
clip_after x = modify $ Segment.num_clip_after x
clip_before x = modify $ Segment.clip_before Segment.num_interpolate x

shift :: X -> Signal kind -> Signal kind
shift x = modify (Segment.shift x)

invert :: Signal kind -> Signal kind
invert = modify Segment.invert

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

-- ** hacks

drop_discontinuity_at :: X -> Control -> Control
drop_discontinuity_at x = modify $ Segment.drop_discontinuity_at x

-- ** scalar transformation

scalar_add, scalar_subtract, scalar_multiply, scalar_divide ::
    Y -> Signal kind -> Signal kind
scalar_add n = map_y_linear (+n)
scalar_subtract n = map_y_linear (subtract n)
scalar_multiply n = map_y_linear (*n)
scalar_divide n = map_y_linear (/n)

-- | Clip signal to never go below the given value.
--
-- This is way more complicated than the piecewise constant version.
scalar_max :: Y -> Signal kind -> Signal kind
scalar_max val sig
    | minimum sig >= val = sig
    | otherwise = modify (Segment.transform_samples go) sig
    where
    go [] = []
    go [Sample x y] = [Sample x (max val y)]
    go (s1@(Sample x1 y1) : s2s@(Sample x2 y2 : sn))
        | y1 < val && y2 < val = Sample x1 val : below (s1 : s2s)
        | y1 >= val && y2 >= val = s1 : go s2s
        | otherwise = case TimeVector.x_at x1 y1 x2 y2 val of
            Nothing
                | y1 < val -> Sample x1 val : go (Sample x2 val : sn)
                | otherwise -> s1 : go s2s
            Just x_val
                | y1 < val -> Sample x1 val : go (Sample x_val val : s2s)
                | otherwise -> s1 : Sample x_val val : below s2s
    -- The first sample is below val, discard until it comes back up again.
    below (Sample x1 y1 : s2s@(Sample x2 y2 : _))
        | y2 < val = below s2s
        | y2 == val = go s2s
        | otherwise = case TimeVector.x_at x1 y1 x2 y2 val of
            -- y1 and y2 are both below, should have been caught above.
            Nothing -> below s2s
            Just x_val -> go (Sample x_val val : s2s)
    below [_] = []
    below [] = []

minimum, maximum :: Signal kind -> Y
minimum = fromMaybe 0 . Segment.minimum . _signal
maximum = fromMaybe 0 . Segment.maximum . _signal

-- | Map Xs.  The slopes will definitely change unless the function is adding
-- a constant, but presumably that's what you want.
map_x :: (X -> X) -> Signal kind -> Signal kind
map_x = modify . Segment.map_x

-- | Map Ys.  This resamples the signal, so it's valid for a nonlinear
-- function.
map_y :: X -> (Y -> Y) -> Signal kind -> Signal kind
map_y srate = modify . Segment.map_y srate

-- | If the function is linear, there's no need to resample.
map_y_linear :: (Y -> Y) -> Signal kind -> Signal kind
map_y_linear = modify . Segment.map_y_linear

map_err :: (Sample Y -> Either err (Sample Y)) -> Signal kind
    -> (Signal kind, [err])
map_err f = first Signal . Segment.map_err f . _signal

-- * special functions

integrate_inverse :: Tempo -> Warp
integrate_inverse = integrate . map_y tempo_srate (1/)

-- | Integrate the signal.
--
-- Since the output will have more samples than the input, this needs
-- a sampling rate.  The sampling rate determines the resolution of the tempo
-- track.  So it can probably be fairly low resolution before having
-- a noticeable impact.
--
-- TODO this is only called after map_y at srate, so it's already been
-- resampled.  Maybe it would be more efficient to remove srate from
-- Segment.integrate.
integrate :: Tempo -> Warp
integrate = Signal . Segment.integrate tempo_srate . _signal

tempo_srate :: X
tempo_srate = RealTime.seconds 10

-- | Total duration of horizontal segments in the warp signal.  These are
-- the places where 'Warp.compose_hybrid' will emit a 1\/1 line.
flat_duration :: Warp -> ScoreTime
flat_duration =
    RealTime.to_score . fst . Vector.foldl' go (0, Segment.Sample 0 0)
        . Segment.to_vector . _signal
    where
    go (!acc, Segment.Sample x0 y0) sample@(Segment.Sample x y)
        | y == y0 = (acc + (x - x0), sample)
        | otherwise = (acc, sample)
