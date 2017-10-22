-- Copyright 2013 Evan Laforge
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
    Signal, sig_vec
    , X, Y, x_to_y, y_to_x, y_to_score, y_to_nn, nn_to_y
    , tempo_srate

    -- * constants
    , invalid_pitch, empty, zero
    , Tempo, Warp, Control, NoteNumber, Display

    -- * construction / deconstruction
    , signal, unsignal, unsignal_unique, set, constant, unfoldr
    , length, null
    , coerce
    , with_ptr
    -- * check
    , check, check_warp

    -- * access
    , at, sample_at, before
    , at_linear, at_linear_extend
    , inverse_at, inverse_at_extend
    , constant_val
    , head, last, uncons
    , Sample(..)
    , minimum, maximum

    -- * transformation
    , merge, concat, interleave, prepend
    , sig_add, sig_subtract, sig_multiply, sig_scale
    , scale, scale_invert
    -- ** scalar transformation
    , sig_max, sig_min, scalar_max, scalar_min, clip_bounds
    , scalar_add, scalar_subtract, scalar_multiply, scalar_divide
    , shift
    , take, drop, drop_while, within
    , drop_at_after, drop_after, drop_before, drop_before_strict, drop_before_at
    , map_x, map_y, map_err

    -- ** special functions
    , compose, compose_hybrid, integrate
    , unwarp, unwarp_fused, invert
    , pitches_share
) where
import qualified Prelude
import Prelude
       hiding (concat, head, last, length, maximum, minimum, null, take, drop)
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as Monad.State

import qualified Data.Vector.Storable as Vector
import qualified Foreign
import qualified Text.Read as Read

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Util.TimeVector as TimeVector
import Util.TimeVector (X, Sample(..))

import qualified Midi.Midi as Midi
import qualified Ui.ScoreTime as ScoreTime
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Global
import Types


-- * types

-- | A Signal is a 'TimeVector.Unboxed' "Util.TimeVector" of 'Y' values, which
-- are just Doubles.  It takes a phantom type parameter to make the signal's
-- intended uses a little clearer.  There are type aliases for the various
-- flavors of signal below, but it really is just documentation and anyone who
-- wants to operate on a generic signal can take a @Signal y@.
newtype Signal y = Signal { sig_vec :: TimeVector.Unboxed }
    deriving (DeepSeq.NFData, Pretty, Eq, Serialize.Serialize)

instance Show (Signal y) where
    show (Signal vec) = "Signal " ++ show (TimeVector.unsignal vec)
instance Read.Read (Signal y) where
    readPrec = do
        Pretty.readWord
        vec <- Read.readPrec
        return $ Signal (TimeVector.signal vec)

instance Monoid (Signal y) where
    mempty = empty
    mappend s1 s2
        | null s1 = s2
        | null s2 = s1
        | otherwise = mconcat [s1, s2]
    mconcat = merge

type Y = Double

modify :: (TimeVector.Unboxed -> TimeVector.Unboxed) -> Signal y -> Signal y
modify f = Signal . f . sig_vec

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
-- 0.05 = 50 Hz = 800b\/sec = 47kb\/min
-- 0.01 = 100 Hz = 1600b\/sec = 94kb\/min
tempo_srate :: X
tempo_srate = RealTime.seconds 0.1


-- * construction / deconstruction

signal :: [(X, Y)] -> Signal y
signal = Signal . TimeVector.signal

-- | The inverse of the 'signal' function.
unsignal :: Signal y -> [(X, Y)]
unsignal = TimeVector.unsignal . sig_vec

unsignal_unique :: Signal y -> [(X, Y)]
unsignal_unique = TimeVector.unsignal_unique . sig_vec

-- | Set the signal value, with a discontinuity.
set :: Maybe Y -> X -> Y -> Signal y
set prev_y x y = Signal $ TimeVector.set prev_y x y

constant :: Y -> Signal y
constant = Signal . TimeVector.constant

unfoldr :: (state -> Maybe ((X, Y), state)) -> state -> Signal y
unfoldr f st = Signal $ TimeVector.unfoldr f st

length :: Signal y -> Int
length = TimeVector.length . sig_vec

null :: Signal y -> Bool
null = TimeVector.null . sig_vec

-- | Sometimes signal types need to be converted.
coerce :: Signal y1 -> Signal y2
coerce (Signal vec) = Signal vec

with_ptr :: Display -> (Foreign.Ptr (Sample Double) -> Int -> IO a) -> IO a
with_ptr sig f = TimeVector.with_ptr (sig_vec sig) $ \sigp ->
    f sigp (TimeVector.length (sig_vec sig))

-- * check

check :: Signal y -> [String]
check = TimeVector.check . sig_vec

-- | Find places where the Warp is non monotonically nondecreasing.
check_warp :: Warp -> [String]
check_warp = reverse . fst . Vector.foldl' check ([], (0, 0, 0)) . sig_vec
    where
    check (warns, (i, prev_x, prev_y)) (Sample x y)
        | x < prev_x = first ("index " <> show i <> ": x decreased: "
            <> show x <> " < " <> show prev_x :) next
        | y < prev_y = first ("index " <> show i <> ": y decreased: "
            <> show y <> " < " <> show prev_y :) next
        | otherwise = next
        where next = (warns, (i + 1, x, y))

-- * access

at :: X -> Signal y -> Y
at x = fromMaybe 0 . TimeVector.at x . sig_vec

sample_at :: X -> Signal y -> Maybe (X, Y)
sample_at x = TimeVector.sample_at x . sig_vec

-- | Find the value immediately before the point.
before :: RealTime -> Signal y -> Y
before x = maybe 0 sy . TimeVector.before x . sig_vec

at_linear :: X -> Signal y -> Y
at_linear x sig = interpolate vec (TimeVector.highest_index x vec)
    where
    vec = sig_vec sig
    interpolate vec i
        | TimeVector.null vec = 0
        | i + 1 >= TimeVector.length vec = y0
        | i < 0 = 0
        | otherwise = TimeVector.y_at x0 y0 x1 y1 x
        where
        (x0, y0) = index i
        (x1, y1) = index (i+1)
    index = TimeVector.to_pair . TimeVector.index vec

-- | This is a version of 'at_linear' that extends the signal on either side
-- with a straight line.  A signal with no samples is a 1:1 line, one with
-- a single sample is a 1:1 line passing through that point, and one with more
-- samples will be extended according to the slope of the two samples at the
-- beginning and end.
--
-- This is used by 'Derive.Score.warp_pos'.
at_linear_extend :: X -> Warp -> Y
at_linear_extend x (Signal vec) = interpolate (TimeVector.highest_index x vec)
    where
    interpolate i
        | TimeVector.null vec = x_to_y x
        | i + 1 >= TimeVector.length vec = if i - 1 < 0
            then TimeVector.y_at x0 y0 (x0+1) (y0+1) x
            else let Sample x_1 y_1 = index (i-1)
                in TimeVector.y_at x_1 y_1 x0 y0 x
        | i < 0 = if TimeVector.length vec == 1
            then TimeVector.y_at x1 y1 (x1+1) (y1+1) x
            else let Sample x2 y2 = index 1 in TimeVector.y_at x1 y1 x2 y2 x
        | otherwise = TimeVector.y_at x0 y0 x1 y1 x
        where
        Sample x0 y0 = index i
        Sample x1 y1 = index (i+1)
    index = TimeVector.index vec

-- | Find the X at which the signal will attain the given Y.  Assumes Y is
-- non-decreasing.  This should be the inverse of 'at_linear'.
--
-- Unlike the other signal functions, this takes a single Y instead of
-- a signal, and as a RealTime.  This is because it's used by the play monitor
-- for the inverse tempo map, and the play monitor polls on intervals defined
-- by IO latency, so even when signals are lazy it would be impossible to
-- generate the input signal without unsafeInterleaveIO.  If I really want to
-- pass a signal, I could pass regular samples and let the monitor interpolate.
inverse_at :: Y -> Warp -> Maybe X
inverse_at y sig
    | i >= TimeVector.length vec || i < 0 = Nothing
    | y1 == y = Just x1
    | otherwise = TimeVector.x_at x0 y0 x1 y1 y
    where
    vec = sig_vec sig
    i = lowest_index_y y vec
    Sample x0 y0 = if i <= 0 then Sample 0 0 else TimeVector.index vec (i-1)
    Sample x1 y1 = TimeVector.index vec i

-- | This is like 'inverse_at', except that if the Y value is past the end
-- of the signal, it extends the signal as far as necessary.  When used for
-- warp composition or unwarping, this means that the parent warp is too small
-- for the child.  Normally this shouldn't happen, but if it does it's
-- sometimes better to make something up than crash.
--
-- The rules for extension are the same as 'at_linear_extend', and this
-- function should be the inverse of that one.  This ensures that if you warp
-- and then unwarp a time, you get your original time back.
inverse_at_extend :: Y -> Warp -> X
inverse_at_extend y (Signal vec)
    | TimeVector.null vec = y_to_x y
    -- Nothing means the line is flat and will never reach Y.  I pick a big
    -- X instead of crashing.
    | otherwise = fromMaybe RealTime.large $ TimeVector.x_at x0 y0 x1 y1 y
    where
    -- Has to be the highest index, or it gets hung up on a flat segment.
    i = index_above_y y vec
    (Sample x0 y0, Sample x1 y1)
        | len == 1 =
            let at0@(Sample x0 y0) = index 0
            in (at0, Sample (x0+1) (y0+1))
        | i >= TimeVector.length vec = (index (i-2), index (i-1))
        | i == 0 = (index 0, index 1)
        | otherwise = (index (i-1), index i)
        where len = TimeVector.length vec
    index = TimeVector.index vec

-- | Just if the signal is constant.
constant_val :: Signal y -> Maybe Y
constant_val = TimeVector.constant_val . sig_vec

head :: Signal y -> Maybe (X, Y)
head = fmap TimeVector.to_pair . TimeVector.head . sig_vec

last :: Signal y -> Maybe (X, Y)
last = fmap TimeVector.to_pair . TimeVector.last . sig_vec

uncons :: Signal y -> Maybe ((X, Y), Signal y)
uncons sig = case TimeVector.uncons (sig_vec sig) of
    Nothing -> Nothing
    Just (Sample x y, vec) -> Just ((x, y), Signal vec)


-- * transformation

merge :: [Signal y] -> Signal y
merge = Signal . TimeVector.merge . map sig_vec

-- | This is like 'merge', but directly concatenates the signals.  It should be
-- more efficient when you know the signals don't overlap.
concat :: [Signal y] -> Signal y
concat = Signal . Vector.concat . map sig_vec

interleave :: Signal y -> Signal y -> Signal y
interleave sig1 sig2 = Signal $
    TimeVector.interleave (sig_vec sig1) (sig_vec sig2)

prepend :: Signal y -> Signal y -> Signal y
prepend s1 s2 = Signal $ TimeVector.prepend (sig_vec s1) (sig_vec s2)

sig_add, sig_multiply :: Control -> Control -> Control
sig_add = sig_op (Just 0) (+)
sig_multiply = sig_op (Just 1) (*)

sig_subtract :: Control -> Control -> Control
sig_subtract sig1 sig2
    -- It turns out subtraction has an identity, but isn't commutative.
    -- Who knew?
    | Just v <- constant_val sig2, v == 0 = sig1
    | otherwise = sig_op Nothing (-) sig1 sig2

sig_scale :: Control -> Control -> Control
sig_scale = sig_op (Just 1) scale

scale :: Y -> Y -> Y
scale x v
    | v >= 0 = Num.scale x 1 v
    | otherwise = Num.scale 0 x (v + 1)

scale_invert :: Y -> Y -> Y
scale_invert old new
    | new >= old = Num.normalize old 1 new
    | otherwise = Num.normalize 0 old new - 1

sig_max, sig_min :: Control -> Control -> Control
sig_max = sig_op Nothing max
sig_min = sig_op Nothing min

-- ** scalar transformation

scalar_add, scalar_subtract, scalar_multiply, scalar_divide ::
    Y -> Signal y -> Signal y
scalar_add n = map_y (+n)
scalar_subtract n = map_y (subtract n)
scalar_multiply n = map_y (*n)
scalar_divide n = map_y (/n)

-- | Clip signal to never go above or below the given value.  Like 'sig_max'
-- and 'sig_min' except the value is scalar.
scalar_max, scalar_min :: Y -> Signal y -> Signal y
scalar_max val = map_y (min val)
scalar_min val = map_y (max val)

minimum, maximum :: Signal y -> Maybe (X, Y)
minimum sig
    | null sig = Nothing
    | otherwise = Just $ TimeVector.to_pair $
        Vector.minimumBy (\a b -> compare (sy a) (sy b)) $ sig_vec sig
maximum sig
    | null sig = Nothing
    | otherwise = Just $ TimeVector.to_pair $
        Vector.maximumBy (\a b -> compare (sy a) (sy b)) $ sig_vec sig

-- | Clip the signal's Y values to lie between (0, 1), inclusive.  Return the
-- half-open ranges during which the Y was out of range, if any.
clip_bounds :: Y -> Y -> Signal y -> (Signal y, [(X, X)])
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

shift :: X -> Signal y -> Signal y
shift 0 = id
shift x = modify (TimeVector.shift x)

take :: Int -> Signal y -> Signal y
take = modify . TimeVector.take

drop :: Int -> Signal y -> Signal y
drop = modify . TimeVector.drop

drop_while :: (Sample Y -> Bool) -> Signal y -> Signal y
drop_while = modify . Vector.dropWhile

within :: X -> X -> Signal y -> Signal y
within start end = modify $ TimeVector.within start end

drop_at_after :: X -> Signal y -> Signal y
drop_at_after = modify . TimeVector.drop_at_after

drop_after :: X -> Signal y -> Signal y
drop_after = modify . TimeVector.drop_after

drop_before :: X -> Signal y -> Signal y
drop_before = modify . TimeVector.drop_before

drop_before_strict :: X -> Signal y -> Signal y
drop_before_strict = modify . TimeVector.drop_before_strict

drop_before_at :: X -> Signal y -> Signal y
drop_before_at = modify . TimeVector.drop_before_at

map_x :: (X -> X) -> Signal y -> Signal y
map_x = modify . TimeVector.map_x

map_y :: (Y -> Y) -> Signal y -> Signal y
map_y = modify . TimeVector.map_y

map_err :: (Sample Y -> Either err (Sample Y)) -> Signal y -> (Signal y, [err])
map_err f = first Signal . TimeVector.map_err f . sig_vec

sig_op :: Maybe Y -- ^ If an identity value is given, I can avoid copying the
    -- whole signal if the other one is a constant identity.
    -> (Y -> Y -> Y) -> Signal y -> Signal y -> Signal y
sig_op (Just identity) _ sig1 sig2
    | Just v <- constant_val sig1, v == identity = sig2
    | Just v <- constant_val sig2, v == identity = sig1
sig_op _ op sig1 sig2 =
    Signal $ TimeVector.sig_op 0 op (sig_vec sig1) (sig_vec sig2)

-- ** special functions

index_above_y :: Y -> TimeVector.Unboxed -> Int
index_above_y y vec = go 0 (TimeVector.length vec)
    where
    go low high
        | low == high = low
        | y >= sy (TimeVector.unsafeIndex vec mid) = go (mid+1) high
        | otherwise = go low mid
        where mid = (low + high) `div` 2

lowest_index_y :: Y -> TimeVector.Unboxed -> Int
lowest_index_y y vec = go 0 (TimeVector.length vec)
    where
    go low high
        | low == high = low
        | y <= sy (TimeVector.unsafeIndex vec mid) = go low mid
        | otherwise = go (mid+1) high
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
compose f = modify $ TimeVector.map_y $ \y -> at_linear (y_to_x y) f
    -- TODO Walking down f would be more efficient, especially once Signal is
    -- lazy.

-- | This is like 'compose', but implements a kind of \"semi-absolute\"
-- composition.  The idea is that it's normal composition until the second
-- signal has a slope of zero.  Normally this would be a discontinuity, but
-- is special cased to force the output to a 1\/1 line.  In effect, it's as
-- if the flat segment were whatever slope is necessary to to generate a slope
-- of 1 when composed with the first signal.
compose_hybrid :: Warp -> Warp -> Warp
compose_hybrid f g = Signal $ run initial $ Vector.generateM (length g) gen
    where
    -- If 'g' starts with a flat segment, I need to start the linear bit in the
    -- right place.
    initial = (at_linear (y_to_x y) f, 0)
        where y = maybe 0 snd (head g)
    run state m = Identity.runIdentity $ Monad.State.evalStateT m state
    -- Where h = f•g:
    -- If g(x_t) == g(x_t-1), then this is a flat segment.
    -- h(x) is simply h(x_t-1) + (x_t - x_t-1), but I have to store an
    -- offset that represents where the signal would be were just right to
    -- produce a slope of 1, so I work backwards:
    -- offset = f-1(h(x)) - g(x_t-1)
    --
    -- If g(x_t) > g(x_t-1), then this is a normal positive slope, and I
    -- have to add the offset: h(x) = f(g(x + offset)).
    --
    -- So the state is (h(x_t-1), offset).
    gen i
        | gy0 == gy = gen_flat gx gx0 gy0
        | otherwise = gen_normal gx gy
        where
        Sample gx gy = Vector.unsafeIndex (sig_vec g) i
        Sample gx0 gy0
            | i == 0 = Sample 0 0
            | otherwise = Vector.unsafeIndex (sig_vec g) (i-1)
    gen_flat gx gx0 gy0 = do
        (y0, _) <- Monad.State.get
        let y = y0 + x_to_y (gx - gx0)
            offset = inverse_at_extend y f - y_to_x gy0
        Monad.State.put (y, offset)
        return $ Sample gx y
    gen_normal gx gy = do
        (_, offset) <- Monad.State.get
        let y = at_linear (y_to_x gy + offset) f
        Monad.State.put (y, offset)
        return $ Sample gx y

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
-- is enough.  To this end, 'Derive.Tempo.extend_signal' will ensure there's
-- a sample at the end of the track.
integrate :: X -> Tempo -> Warp
integrate srate = coerce . modify (TimeVector.concat_map_accum 0 go final 0)
    where
    go = integrate_segment srate
    final accum (Sample x _) = [Sample x accum]

integrate_segment :: X -> Y -> X -> Y -> X -> Y -> (Y, [Sample Y])
integrate_segment srate accum x0 y0 x1 _y1
    | x0 >= x1 = (accum, [])
    | otherwise = (y_at x1, [Sample x (y_at x) | x <- xs])
    where
    xs = Seq.range' x0 x1 srate
    y_at x = accum + x_to_y (x-x0) * y0

warp :: Warp -> Control -> Control
warp w = modify $ TimeVector.map_x $ \x -> y_to_x (at_linear x w)

-- | Take a Control in RealTime and unwarp it back to ScoreTime.  The only
-- reason to do this is to display in the UI, so the return type is Display.
unwarp :: Warp -> Control -> Display
unwarp w = coerce . warp (invert w)

-- | Previously, 'unwarp' was called as
-- @Signal.unwarp (Score.warp_to_signal warp) control@.  This converts the
-- entire @warp@, which is often large thanks to the sampling rate required by
-- 'integrate', for the sake of unwarping @control@, which is often very small,
-- thanks to track slicing, and does so again and again.  Fusion should
-- take care of making the warp conversion just as efficient as manually
-- applying the shift and stretch, but presumably can't handle only inverting
-- the part of the warp needed to unwarp the control, becasue the signal is
-- strict.
unwarp_fused :: Warp -> RealTime -> RealTime -> Control -> Display
unwarp_fused w shift stretch = coerce . modify (TimeVector.map_x unwarp)
    where unwarp x = (inverse_at_extend (x_to_y x) w - shift) / stretch

invert :: Warp -> Warp
invert = modify $ Vector.map $ \(Sample x y) -> Sample (y_to_x y) (x_to_y x)

--- * comparison

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
--
-- This function will be confused by multiple samples at the same time, so
-- don't do that.
pitches_share :: Bool -> X -> X
    -> Midi.Key -> NoteNumber -> Midi.Key -> NoteNumber -> Bool
pitches_share in_decay start end initial1 sig1 initial2 sig2
    | not in_decay && initial1 == initial2 = False
    | otherwise = pitch_eq (at start sig1) (at start sig2)
        && pitch_eq (at end sig1) (at end sig2)
        && signals_share pitch_eq start in1 in2
    where
    in1 = TimeVector.within start end (sig_vec sig1)
    in2 = TimeVector.within start end (sig_vec sig2)
    pitch_eq = nns_share initial1 initial2

-- | I need to sample points from start to end, including the start and the
-- end.  Unfortunately it's not as simple as it seems it should be, especially
-- since this function is a hotspot and must be efficient.
--
-- TimeVector.within may return samples before start to get the proper value so
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
