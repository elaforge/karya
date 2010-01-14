{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, EmptyDataDecls #-}
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
    means that the common case of recorded midi controls takes twice as much
    data, since a flat segment must be expressed as [(x0, y0), (x1, y0), (x2,
    y1), ...].  This will be bad for recorded midi controls, but I may wind
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
module Perform.Signal (
    Signal(Signal), sig_vec
    , X, Y, x_to_y, y_to_x, max_x, max_y, default_srate, tempo_srate
    , invalid_pitch, empty
    , Tempo, Warp, Control, NoteNumber, Display

    , signal, constant, track_signal, Method(..), Segment
    , unpack, to_track_samples
    , log_signal
    , coerce

    , at, at_linear, sample

    , sig_add, sig_subtract, sig_multiply
    , sig_max, sig_min, clip_max, clip_min
    , scalar_add, scalar_subtract, scalar_multiply, scalar_divide
    , shift, scale
    , truncate
    , map_x, map_y

    , inverse_at, compose, integrate
    , integrate_segment -- export for testing

    , equal, pitches_share
) where
import Prelude hiding (truncate)
import qualified Control.Arrow as Arrow
import qualified Data.StorableVector as V
import qualified Foreign.Storable as Storable

import qualified Util.Num as Num
import qualified Util.Log as Log

import Ui
import qualified Ui.Track as Track

import qualified Perform.Timestamp as Timestamp
import qualified Perform.SignalBase as SignalBase
import Perform.SignalBase (Method(..), Segment, max_x, default_srate)


-- * types

newtype Signal y = Signal { sig_vec :: SignalBase.SigVec Y }
    -- The Eq instance is only for tests, since it may be quite expensive on
    -- a real signal.
    deriving (Eq)

modify_vec :: (SignalBase.SigVec Y -> SignalBase.SigVec Y)
    -> Signal y0 -> Signal y1
modify_vec f = Signal . f . sig_vec

type X = SignalBase.X
type Y = Double
instance SignalBase.Signal Y

-- | This is the type of performer-interpreted controls that go into the
-- event's control map.
type Control = Signal ControlSig
data ControlSig

-- | A tempo is a normal Control signal, except that instead of going into the
-- control map, it gets turned into a Warp and goes into the warp map.
type Tempo = Signal TempoSig
data TempoSig

-- | A tempo warp maps score time to global time.
type Warp = Signal WarpSig
data WarpSig

-- | This is the type of pitch signals used by the performer, after the scale
-- has been factored out.
type NoteNumber = Signal NoteNumberSig
data NoteNumberSig

-- | This is the type of signals which are sent to the UI for display.
type Display = Signal DisplaySig
data DisplaySig

instance Storable.Storable (X, Y) where
    sizeOf _ = Storable.sizeOf (undefined :: TrackPos)
        + Storable.sizeOf (undefined :: Double)
    alignment _ = Storable.alignment (undefined :: Double)
    poke cp (a, b) = Storable.pokeByteOff cp 0 a >> Storable.pokeByteOff cp 8 b
    peek cp = do
        a <- Storable.peekByteOff cp 0 :: IO TrackPos
        b <- Storable.peekByteOff cp 8 :: IO Double
        return (a, b)

instance SignalBase.Y Y where
    zero_y = 0
    y_at x0 y0 x1 y1 x = y_at (x_to_y x0) y0 (x_to_y x1) y1 (x_to_y x)
    project y0 y1 at = Num.scale y0 y1 at

instance Show (Signal y) where
    show (Signal vec) = "Signal " ++ show (V.unpack vec)

x_to_y :: X -> Y
x_to_y (TrackPos x) = x
y_to_x :: Y -> X
y_to_x = TrackPos

-- * constants

max_y :: Y
max_y = x_to_y SignalBase.max_x

-- | A pitch that shouldn't be played.  Used for a non-existent pitch or one
-- that goes out of the range of its scale.
invalid_pitch :: Y
invalid_pitch = -1

empty :: Signal y
empty = signal []

-- | Signal composition, used by warps, is really tricky without a constant
-- srate.  Since 'integrate' is the way to generate 'Warp's, ensure they are
-- at this srate by passing this to integrate.
--
-- 0.05 = 50 Hz = 800b/sec = 47kb/min
-- 0.01 = 100 Hz = 1600b/sec = 94kb/min
tempo_srate :: X
    -- TODO resolution is very low for the moment since I have neither lazy
    -- signals nor a graphical way to log signals yet
tempo_srate = 0.1

-- * construction / deconstruction

signal :: [(X, Y)] -> Signal y
signal ys = Signal (V.pack ys)

constant :: Y -> Signal y
constant n = signal [(0, n)]

track_signal :: X -> [SignalBase.Segment] -> Signal y
track_signal srate segs = Signal (SignalBase.track_signal srate segs)

-- | Used for tests.
unpack :: Signal y -> [(X, Y)]
unpack = V.unpack . sig_vec

-- | A hack to log a signal.  This way it can be extracted later and displayed
-- in a format that's nicer than a huge log line.
log_signal :: Signal y -> Log.Msg -> Log.Msg
log_signal sig msg =
    msg { Log.msg_signal = [(x, y) | (TrackPos x, y) <- unpack sig] }

-- | TODO This is used by the signal deriver and is inefficient.  I should be
-- passing a pointer.
to_track_samples :: Signal y -> Track.Samples
to_track_samples = Track.samples . unpack

-- | Sometimes signal types need to be converted.
coerce :: Signal y0 -> Signal y1
coerce (Signal vec) = Signal vec

-- * access

at, at_linear :: X -> Signal y -> Y
at x sig = SignalBase.at x (sig_vec sig)
at_linear x sig = SignalBase.at_linear x (sig_vec sig)

sample :: X -> Signal y -> [(X, Y)]
sample start sig = SignalBase.sample start (sig_vec sig)


-- * transformation

sig_add, sig_subtract, sig_multiply :: Control -> Control -> Control
sig_add = sig_op (+)
sig_subtract = sig_op (-)
sig_multiply = sig_op (*)

sig_max, sig_min :: Control -> Control -> Control
sig_max = sig_op max
sig_min = sig_op min

-- ** scalar transformation

scalar_add, scalar_subtract, scalar_multiply, scalar_divide ::
    Y -> Control -> Control
scalar_add n = map_y (+n)
scalar_subtract n = map_y (subtract n)
scalar_multiply n = map_y (*n)
scalar_divide n = map_y (/n)

-- | Clip signal to never go above or below the given value.  Like 'sig_max'
-- and 'sig_min' except relative to a scalar value.
clip_max, clip_min :: Y -> Signal y -> Signal y
clip_max val = modify_vec (V.map (Arrow.second (min val)))
clip_min val = modify_vec (V.map (Arrow.second (max val)))

shift :: X -> Signal y -> Signal y
shift x = modify_vec (SignalBase.shift x)

scale :: Y -> Signal y -> Signal y
scale mult vec
    | mult <= 0 = error $ "scale: called with mult<=0: " ++ show mult
    | mult == 1 = vec
    | otherwise = map_y (*mult) vec

truncate :: X -> Signal y -> Signal y
truncate x = modify_vec (SignalBase.truncate x)

map_x :: (X -> X) -> Signal y -> Signal y
map_x f = modify_vec (SignalBase.map_x f)

map_y :: (Y -> Y) -> Signal y -> Signal y
map_y f = modify_vec (SignalBase.map_y f)

sig_op :: (Y -> Y -> Y) -> Signal y -> Signal y -> Signal y
sig_op op sig0 sig1 =
    Signal (SignalBase.sig_op op (sig_vec sig0) (sig_vec sig1))

-- ** special functions

-- | Find the X at which the signal will attain the given Y.  Assumes Y is
-- non-decreasing.
--
-- Unlike the other signal functions, this takes a single Y instead of
-- a signal, and as a Timestamp.  This is because it's used by the play updater
-- for the inverse tempo map, and the play updater polls on intervals defined
-- by IO latency, so even when signals are lazy it would be impossible to
-- generate the input signal without unsafeInterleaveIO.  If I really want to
-- pass a signal, I could pass regular samples and let the updater interpolate.
--
-- This uses a bsearch on the vector, which is only reasonable as long as
-- its strict.  When I switch to lazy vectors, I'll have to thread the tails.
inverse_at :: Warp -> Timestamp.Timestamp -> Maybe X
inverse_at sig ts
    | i >= V.length vec = Nothing
    | y1 == y = Just x1
    | otherwise = Just $ y_to_x $ x_at (x_to_y x0) y0 (x_to_y x1) y1 y
    where
    vec = sig_vec sig
    y = x_to_y (Timestamp.to_track_pos ts)
    i = SignalBase.bsearch_on vec snd y
        -- This can create x0==x1, but y1 should == y in that case.
    (x0, y0) = if i-1 < 0 then (0, 0) else V.index vec (i-1)
    (x1, y1) = V.index vec i

-- | Compose the first signal with the second.
compose :: Warp -> Warp -> Warp
compose f g = Signal $ SignalBase.map_y go (sig_vec g)
    where go y = SignalBase.at_linear (y_to_x y) (sig_vec f)
    -- TODO Walking down f would be more efficient, especially once Signal is
    -- lazy.

-- | Integrate the signal.
--
-- The sample points are linear interpolated.
integrate :: X -> Tempo -> Warp
integrate srate = modify_vec (SignalBase.map_signal_accum go final 0)
    where
    go accum x0 y0 x1 y1 = integrate_segment srate accum x0 y0 x1 y1
    -- TODO for now only append a few seconds of samples to extend the
    -- integration.  This means a tempo track will only extend this far
    -- past the last sample, which is clearly not good, but when signals are
    -- lazy this can extend indefinitely and the problem goes away.
    padding = 30
    final accum (x, y) = snd $ integrate_segment srate accum x y (x+padding) y

integrate_segment :: X -> Y -> X -> Y -> X -> Y -> (Y, [(X, Y)])
integrate_segment srate accum x0 y0 x1 _y1
    | x0 >= x1 = (accum, [])
    | otherwise = (y_at x1, [(x, y_at x) | x <- xs])
    where
    xs = SignalBase.range False x0 x1 srate
    y_at x = accum + x_to_y (x-x0) * y0

--- * comparison

equal :: X -> X -> Signal y -> Signal y -> Bool
equal x0 x1 sig0 sig1 = SignalBase.equal x0 x1 (sig_vec sig0) (sig_vec sig1)

-- | Can the pitch signals share a channel within the given range?
--
-- This operates on control signals, not PitchSignals.  By the time this is
-- called, the PitchSignal has been converted to a regular Signal.
--
-- Pitch is complicated.  Like other controls, if the pitch curves are
-- different they may not share a channel.  However, if the pitch curves
-- are integral transpositions of each other, and the transposition is not
-- 0, they should share.  Unless the overlap occurs during the decay of one or
-- both notes, at which point 0 transposition is ok.
--
-- TODO this is actually a MIDI notion, so it should go in Perform.Midi
pitches_share :: Bool -> X -> X -> NoteNumber -> NoteNumber -> Bool
pitches_share in_decay start end sig0 sig1 =
    pitch_share in_decay (at start sig0) (at start sig1)
        && pitch_share in_decay (at end sig0) (at end sig1)
        && all pitch_eq samples
    where
    -- Unlike 'equal' I do resample, because there's a high chance of notes
    -- matching but not lining up in time.
    samples = SignalBase.resample_to_list
        (SignalBase.within start end (sig_vec sig0))
        (SignalBase.within start end (sig_vec sig1))
    pitch_eq (_, ay, by) = pitch_share in_decay ay by

-- | Only compare out to cents, since differences below that aren't really
-- audible.
pitch_share :: Bool -> Y -> Y -> Bool
pitch_share in_decay v0 v1 =
    (in_decay || fst (properFraction v0) /= fst (properFraction v1))
        && f v0 == f v1
    where f v = floor (snd (properFraction v) * 1000)


-- * util

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



{-

-- TODO unused but maybe useful some day
is_constant :: TrackPos -> TrackPos -> Signal -> Bool
is_constant start end sig =
    all (== (Seq.mhead 0 id vals)) vals && at start sig == at end sig
    where vals = unpack_vals (within start end sig)

-}
