{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- NFData instance
{- | Sample values are doubles, which means each point in the signal is 8*2
    bytes.  The double resolution is overkill for the value, but float would
    be too small for time given the time stretching.

    TODO split this into Float and Double versions since only Warp really
    needs Double.  Or does Warp really need Double?
-}
module Perform.Signal2 (
    -- * types
    Signal, sig_vec
    , X, Y, x_to_y, y_to_real, y_to_score, y_to_nn, nn_to_y
    , tempo_srate

    -- * constants
    , invalid_pitch, empty, zero
    , Tempo, Warp, Control, NoteNumber, Display

    -- * construction / deconstruction
    , signal, unsignal
    , constant, length, null
    , log_signal
    , coerce
    , to_foreign_ptr

    -- * access
    , at, at_linear, is_constant
    , first, last

    -- * transformation
    , merge
    , sig_add, sig_subtract, sig_multiply
    -- ** scalar transformation
    , sig_max, sig_min, scalar_max, scalar_min, clip_bounds
    , scalar_add, scalar_subtract, scalar_multiply, scalar_divide
    , shift, scale
    , truncate, drop_before
    , map_x, map_y

    -- ** special functions
    , inverse_at, compose, integrate
    , pitches_share
) where
import qualified Prelude
import Prelude hiding (last, truncate, length, null)
import qualified Control.Arrow as Arrow
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Foreign
import qualified Text.Read as Read

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TimeVector as V

import qualified Midi.Midi as Midi
import qualified Ui.ScoreTime as ScoreTime
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Types


-- * types

-- These are boxed, I want unboxed.
newtype Signal y = Signal { sig_vec :: V.Unboxed }
    deriving (DeepSeq.NFData, Pretty.Pretty, Eq)

instance Show (Signal y) where
    show (Signal vec) = "Signal " ++ show (V.unsignal vec)
instance Read.Read (Signal y) where
    readPrec = do
        Pretty.read_word
        vec <- Read.readPrec
        return $ Signal (V.signal vec)

instance Monoid.Monoid (Signal y) where
    mempty = empty
    mappend s1 s2 = Monoid.mconcat [s1, s2]
    mconcat = merge

type X = V.X
type Y = Double

modify_vec :: (V.Unboxed -> V.Unboxed) -> Signal y -> Signal y
modify_vec f = Signal . f . sig_vec

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

y_to_real :: Y -> X
y_to_real = RealTime.seconds

-- | Some control signals may be interpreted as score time.
y_to_score :: Y -> ScoreTime
y_to_score = ScoreTime.double

y_to_nn :: Y -> Pitch.NoteNumber
y_to_nn = Pitch.NoteNumber

nn_to_y :: Pitch.NoteNumber -> Y
nn_to_y (Pitch.NoteNumber nn) = nn

-- * constants

-- | A pitch that shouldn't be played.  Used for a non-existent pitch or one
-- that goes out of the range of its scale.
--
-- This actually has to be 0 because that's also what 'at' returns before the
-- first sample.
invalid_pitch :: Y
invalid_pitch = 0

empty :: Signal y
empty = signal []

zero :: Signal y
zero = signal [(0, 0)]

-- | Signal composition, used by warps, is really tricky without a constant
-- srate.  Since 'integrate' is the way to generate 'Warp's, ensure they are
-- at this srate by passing this to integrate.
--
-- 0.05 = 50 Hz = 800b/sec = 47kb/min
-- 0.01 = 100 Hz = 1600b/sec = 94kb/min
tempo_srate :: X
tempo_srate = RealTime.seconds 0.1


-- * construction / deconstruction

signal :: [(X, Y)] -> Signal y
signal = Signal . V.signal

-- | The inverse of the 'signal' function.
unsignal :: Signal y -> [(X, Y)]
unsignal = V.unsignal . sig_vec

constant :: Y -> Signal y
constant n = signal [(0, n)]

length :: Signal y -> Int
length = V.length . sig_vec

null :: Signal y -> Bool
null = V.null . sig_vec

-- | A hack to log a signal.  This way it can be extracted later and displayed
-- in a format that's nicer than a huge log line.
log_signal :: Signal y -> Log.Msg -> Log.Msg
log_signal sig msg = msg { Log.msg_signal =
    map (Arrow.first RealTime.to_seconds) (unsignal sig) }

-- | Sometimes signal types need to be converted.
coerce :: Signal y0 -> Signal y1
coerce (Signal vec) = Signal vec

to_foreign_ptr :: Display -> (Foreign.ForeignPtr (V.Sample Y), Int)
to_foreign_ptr = V.to_foreign_ptr . sig_vec

-- * access

at :: X -> Signal y -> Y
at x sig = Maybe.fromMaybe 0 $ V.at x (sig_vec sig)

at_linear :: X -> Signal y -> Y
at_linear x sig =
    interpolate x (sig_vec sig) (V.highest_index x (sig_vec sig))
    where
    interpolate x vec i
        | V.null vec = 0
        | i + 1 >= V.length vec = y0
        | i < 0 = 0
        | otherwise = V.y_at x0 y0 x1 y1 x
        where
        (x0, y0) = V.to_pair $ V.index vec i
        (x1, y1) = V.to_pair $ V.index vec (i+1)

is_constant :: Signal y -> Bool
is_constant sig = case V.viewL (sig_vec sig) of
    Nothing -> True
    Just (V.Sample x0 y0, rest)
        | x0 == 0 -> V.all ((==y0) . V.sy) rest
        | otherwise -> V.all ((==0) . V.sy) (sig_vec sig)

first :: Signal y -> Maybe (X, Y)
first = fmap V.to_pair . V.head . sig_vec

last :: Signal y -> Maybe (X, Y)
last = fmap V.to_pair . V.last . sig_vec


-- * transformation

merge :: [Signal y] -> Signal y
merge = Signal . V.merge . map sig_vec

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
-- and 'sig_min' except the value is scalar.
scalar_max, scalar_min :: Y -> Signal y -> Signal y
scalar_max val = map_y (min val)
scalar_min val = map_y (max val)

-- | Clip the signal's Y values to lie between (0, 1), inclusive.  Return the
-- half-open ranges during which the Y was out of range, if any.
clip_bounds :: Signal y -> (Signal y, [(X, X)])
clip_bounds sig = (clipped, reverse out_of_range)
    where
    clipped = if Prelude.null out_of_range then sig
        else map_y (Num.clamp low high) sig
    (ranges, in_clip) = V.foldl' go ([], Nothing) (sig_vec sig)
    out_of_range = case (in_clip, last sig) of
        (Just start, Just (end, _)) -> (start, end) : ranges
        _ -> ranges
    go state@(accum, Nothing) (V.Sample x y)
        | y < low || y > high = (accum, Just x)
        | otherwise = state
    go state@(accum, Just start) (V.Sample x y)
        | y < low || y > high = state
        | otherwise = ((start, x) : accum, Nothing)
    low = 0
    high = 1

shift :: X -> Signal y -> Signal y
shift 0 = id
shift x = modify_vec (V.shift x)

scale :: Y -> Signal y -> Signal y
scale mult vec
    | mult <= 0 = error $ "scale: called with mult<=0: " ++ show mult
    | mult == 1 = vec
    | otherwise = map_y (*mult) vec

truncate :: X -> Signal y -> Signal y
truncate = modify_vec . V.truncate

drop_before :: X -> Signal y -> Signal y
drop_before = modify_vec . V.drop_before

map_x :: (X -> X) -> Signal y -> Signal y
map_x = modify_vec . V.map_x

map_y :: (Y -> Y) -> Signal y -> Signal y
map_y = modify_vec . V.map_y

sig_op :: (Y -> Y -> Y) -> Signal y -> Signal y -> Signal y
sig_op op sig0 sig1 = Signal $ V.sig_op 0 op (sig_vec sig0) (sig_vec sig1)

-- ** special functions

-- | Find the X at which the signal will attain the given Y.  Assumes Y is
-- non-decreasing.
--
-- Unlike the other signal functions, this takes a single Y instead of
-- a signal, and as a RealTime.  This is because it's used by the play updater
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
    | otherwise = Just $ V.x_at x0 y0 x1 y1 y
    where
    vec = sig_vec sig
    y = x_to_y pos
    i = bsearch_y y vec
        -- This can create x0==x1, but y1 should == y in that case.
    V.Sample x0 y0 = if i-1 < 0 then V.Sample 0 0 else V.index vec (i-1)
    V.Sample x1 y1 = V.index vec i

bsearch_y :: Y -> V.Unboxed -> Int
bsearch_y y vec = go vec 0 (V.length vec)
    where
    go vec low high
        | low == high = low
        | y <= V.sy (V.unsafeIndex vec mid) = go vec low mid
        | otherwise = go vec (mid+1) high
        where mid = (low + high) `div` 2


-- | Compose the first signal with the second.
--
-- Actually, only the X points from the first warp are used in the output, so
-- the input signals must be at a constant sample rate.  This is different
-- from the variable sampling used all the other signals, but is compatible
-- with the output of 'integrate'.
--
-- It also means that the output will have length equal to that of the first
-- argument.  Since the second argument is likely the warp of a sub-block,
-- it will be shorter, and hence not result in a warp that is too short for
-- its score.
--
-- TODO That also implies there's wasted work when warp outside of the
-- sub-block's range is calculated.  Solutions to that are either to clip the
-- output to the length of the second argument (but this will cause incorrect
-- results if the sub-block wants RealTime outside its range), or, once again,
-- to make signals lazy.
--
-- TODO Wait, what if the warps don't line up at 0?  Does that happen?
compose :: Warp -> Warp -> Warp
compose f g = Signal $ V.map_y go (sig_vec g)
    where go y = at_linear (y_to_real y) f
    -- TODO Walking down f would be more efficient, especially once Signal is
    -- lazy.

-- | Integrate the signal.
--
-- Since the output will have more samples than the input, this needs
-- a sampling rate.  The sampling rate determines the resolution of the tempo
-- track.  So it can probably be fairly low resolution before having
-- a noticeable impact.
--
-- The last sample of a signal is supposed to extend indefinitely, which
-- means that the output of 'integrate' should extend indefinitely at
-- a constant slope.  But since signals are strict, I can't have infinite
-- signals.  So this integrate will only be accurate up until the final sample
-- of the tempo given, and it's up to the caller to ensure that this range
-- is enough.  To this end, the tempo track deriver in "Derive.Control" has
-- a hack to ensure a sample at the end of the track.
integrate :: X -> Tempo -> Warp
integrate srate = coerce . modify_vec (V.concat_map_accum 0 go final 0)
    where
    go accum x0 y0 x1 y1 = integrate_segment srate accum x0 y0 x1 y1
    final accum (V.Sample x _) = [V.Sample x accum]

integrate_segment :: X -> Y -> X -> Y -> X -> Y -> (Y, [V.Sample Y])
integrate_segment srate accum x0 y0 x1 _y1
    | x0 >= x1 = (accum, [])
    | otherwise = (y_at x1, [V.Sample x (y_at x) | x <- xs])
    where
    xs = Seq.range' x0 x1 srate
    y_at x = accum + x_to_y (x-x0) * y0

--- * comparison

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
    in0 = V.within start end (sig_vec sig0)
    in1 = V.within start end (sig_vec sig1)
    -- I need to sample points from start to end, including the start and the
    -- end.  Unfortunately it's not as simple as it seems it should be,
    -- especially since this function is a hotspot and must be efficient.
    --
    -- V.within may return samples before start to get the proper
    -- value so I have to drop them before testing.  Start itself is tested
    -- explicitly above.
    samples = dropWhile (\(t, _, _) -> t <= start) $
        V.resample_to_list 0 in0 in1
    pitch_eq (_, ay, by) = floor ((ay - fromIntegral initial0) * 1000)
        == floor ((by - fromIntegral initial1) * 1000)
