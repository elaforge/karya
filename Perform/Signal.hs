{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- NFData instance
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
    , X, Y, x_to_y, y_to_real, y_to_score, max_x, max_y
    , default_srate, tempo_srate
    , invalid_pitch, empty
    , Tempo, Warp, Control, NoteNumber, Display

    , signal, constant
    , length, null
    , unsignal
    , log_signal
    , coerce

    , at, at_linear, is_constant, sample
    , first, last

    , merge
    , sig_add, sig_subtract, sig_multiply
    , sig_max, sig_min, clip_max, clip_min, clip_bounds
    , scalar_add, scalar_subtract, scalar_multiply, scalar_divide
    , shift, scale
    , truncate, drop_before
    , map_x, map_y

    , inverse_at, compose, integrate
    , integrate_segment -- export for testing

    , equal, pitches_share
) where
import Prelude hiding (last, truncate, length, null)
import qualified Control.Arrow as Arrow
import qualified Prelude
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Monoid as Monoid
import qualified Data.StorableVector as V
import qualified Foreign.Storable as Storable

import qualified Util.Log as Log
import qualified Util.Num as Num

import qualified Midi.Midi as Midi

import Ui

import qualified Perform.SignalBase as SignalBase
import Perform.SignalBase (max_x, default_srate)


-- * types

newtype Signal y = Signal { sig_vec :: SignalBase.SigVec Y }
    -- The Eq instance is only for tests, since it may be quite expensive on
    -- a real signal.
    deriving (Eq, DeepSeq.NFData)

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

instance Storable.Storable (X, Y) where
    sizeOf _ = Storable.sizeOf (undefined :: RealTime)
        + Storable.sizeOf (undefined :: Double)
    alignment _ = Storable.alignment (undefined :: Double)
    poke cp (a, b) = Storable.pokeByteOff cp 0 a >> Storable.pokeByteOff cp 8 b
    peek cp = do
        a <- Storable.peekByteOff cp 0 :: IO RealTime
        b <- Storable.peekByteOff cp 8 :: IO Double
        return (a, b)

instance SignalBase.Y Y where
    zero_y = 0
    to_double = id

instance Show (Signal y) where
    show (Signal vec) = "Signal " ++ show (SignalBase.unsignal vec)

instance Monoid.Monoid (Signal y) where
    mempty = empty
    mappend s1 s2 = Monoid.mconcat [s1, s2]
    mconcat = merge

x_to_y :: X -> Y
x_to_y (RealTime x) = x

y_to_real :: Y -> X
y_to_real = RealTime

-- | Some control signals may be interpreted as score time.
y_to_score :: Y -> ScoreTime
y_to_score = ScoreTime

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
signal ys = Signal (SignalBase.signal ys)

constant :: Y -> Signal y
constant n = signal [(0, n)]

length :: Signal y -> Int
length = V.length . sig_vec

null :: Signal y -> Bool
null = V.null . sig_vec

-- | A hack to log a signal.  This way it can be extracted later and displayed
-- in a format that's nicer than a huge log line.
log_signal :: Signal y -> Log.Msg -> Log.Msg
log_signal sig msg =
    msg { Log.msg_signal = [(x, y) | (RealTime x, y) <- unsignal sig] }

-- | Used for tests.
unsignal :: Signal y -> [(X, Y)]
unsignal = SignalBase.unsignal . sig_vec

-- | Sometimes signal types need to be converted.
coerce :: Signal y0 -> Signal y1
coerce (Signal vec) = Signal vec

-- * access

at, at_linear :: X -> Signal y -> Y
at x sig = SignalBase.at x (sig_vec sig)
at_linear x sig = SignalBase.at_linear x (sig_vec sig)

is_constant :: Signal y -> Bool
is_constant (Signal vec) = case V.viewL vec of
    Nothing -> True
    Just ((_, y0), rest) -> V.all ((==y0) . snd) rest

sample :: X -> Signal y -> [(X, Y)]
sample start sig = SignalBase.sample start (sig_vec sig)

first :: Signal y -> Maybe (X, Y)
first = fmap fst . V.viewL . sig_vec

last :: Signal y -> Maybe (X, Y)
last = fmap snd . V.viewR . sig_vec

-- * transformation

merge :: [Signal y] -> Signal y
merge = Signal . SignalBase.merge . map sig_vec

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

-- | Clip the signal's Y values to lie between (0, 1), inclusive.  Return the
-- half-open ranges during which the Y was out of range, if any.
clip_bounds :: Signal y -> (Signal y, [(X, X)])
clip_bounds sig = (clipped, reverse out_of_range)
    where
    clipped = if Prelude.null out_of_range then sig
        else Signal $ V.map (Arrow.second (Num.clamp low high)) (sig_vec sig)
    (ranges, in_clip) = V.foldl' go ([], Nothing) (sig_vec sig)
    out_of_range = case (in_clip, last sig) of
        (Just start, Just (end, _)) -> (start, end) : ranges
        _ -> ranges
    go state@(accum, Nothing) (x, y)
        | y < low || y > high = (accum, Just x)
        | otherwise = state
    go state@(accum, Just start) (x, y)
        | y < low || y > high = state
        | otherwise = ((start, x) : accum, Nothing)
    low = 0
    high = 1

shift :: X -> Signal y -> Signal y
shift 0 = id
shift x = modify_vec (SignalBase.shift x)

scale :: Y -> Signal y -> Signal y
scale mult vec
    | mult <= 0 = error $ "scale: called with mult<=0: " ++ show mult
    | mult == 1 = vec
    | otherwise = map_y (*mult) vec

truncate :: X -> Signal y -> Signal y
truncate x = modify_vec (SignalBase.truncate x)

drop_before :: X -> Signal y -> Signal y
drop_before x = modify_vec (SignalBase.drop_before x)

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
inverse_at :: RealTime -> Warp -> Maybe X
inverse_at pos sig
    | i >= V.length vec = Nothing
    | y1 == y = Just x1
    | otherwise = Just $ y_to_real $
        SignalBase.x_at (x_to_y x0) y0 (x_to_y x1) y1 y
    where
    vec = sig_vec sig
    y = x_to_y pos
    i = SignalBase.bsearch_on vec snd y
        -- This can create x0==x1, but y1 should == y in that case.
    (x0, y0) = if i-1 < 0 then (0, 0) else V.index vec (i-1)
    (x1, y1) = V.index vec i

-- | Compose the first signal with the second.
compose :: Warp -> Warp -> Warp
compose f g = Signal $ SignalBase.map_y go (sig_vec g)
    where go y = SignalBase.at_linear (y_to_real y) (sig_vec f)
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
    padding = 100
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
--
-- This function will be confused by multiple samples at the same time, so
-- don't do that.
pitches_share :: Bool -> X -> X
    -> Midi.Key -> NoteNumber -> Midi.Key -> NoteNumber -> Bool
pitches_share in_decay start end initial0 sig0 initial1 sig1
    | not in_decay && initial0 == initial1 = False
    | otherwise = all pitch_eq ((start, at start sig0, at start sig1)
        : (end, at end sig0, at end sig1) : samples)
    where
    in0 = SignalBase.within start end (sig_vec sig0)
    in1 = SignalBase.within start end (sig_vec sig1)
    -- I need to sample points from start to end, including the start and the
    -- end.  Unfortunately it's not as simple as it seems it should be,
    -- especially since this function is a hotspot and must be efficient.
    --
    -- SignalBase.within may return samples before start to get the proper
    -- value so I have to drop them before testing.
    samples = dropWhile (\(t, _, _) -> t < start) $
        SignalBase.resample_to_list in0 in1
    pitch_eq (_, ay, by) = floor ((ay - fromIntegral initial0) * 1000)
        == floor ((by - fromIntegral initial1) * 1000)
