{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-
    pitch signal is in scale degrees
    relative pitch tracks have a scale, and convert oct/deg to degrees

    But if this is the case, a linear slide will no longer sound linear.  For that,
    pitch signal needs to be equal tempered.  But then I can't transpose a signal.

    The signal must stay as segments until actual performance.  Adding together
    segments is not clear.  How to add exp to linear?  Well, *relative* pitches can
    be added easily.  So a pitch signal is really
    (absolute_segements, relative_sig).

    1.0 -> 1.0  or  1.0
    1.5 -> 1.5  or  1.4
    2.0 -> 2.0  or  1.8
    2.5 -> 2.25 or  2.6
    3.0 -> 2.5  or  3.0

    Convert segment ends to nn, *then* interpolate.

    Ok but how to add relative to absolute?  I reduce absolute

    [(1, Lin, 2), (2, Lin, 3)] ->
    [(1, (1, 2, 0)), (1.1, (1, 2, 0.1), ..,
        (2, (2, 3, 0)), (2.1, (2, 3, 0.1)), ...]

        (pos, (from, to, at))

    relative can also be reduced to this, and added to 'from' 'to'
    relative needs to know the scale only so it knows what an octave means


    It's a little awkward how this is a whole different module, but I think I can
    put the shared code in one place:

    Methods, segments, and some of the interpolation code

    _resample
    at, at_linear
    sample

    sig math: add, sub, max, min
    shift, stretch
    truncate

    Pitch signal moves out of event_controls into its own attr.

    (from, to, at), how about (from, at)

    (4, 4, 0), (4, 4, 0.1), ...

    (0, (2, 0, 0)), (1, (2, 0, 1)), (2, (2, 0, 0)),

    Compress identical (from, to)?  But StorableVector can't do variable length
    encoding.

    TODO

    - Score.Event gets a event_pitch field
    - Derive uses PitchSignal for absolute pitch, RelativePitchSignal for relative
    ones
    - Perform.Midi.Convert converts PitchSignal to Signal
-}
module Perform.PitchSignal (
    PitchSignal, sig_scale, sig_vec
    , RelativePitchSignal
    , X, Y, max_x, default_srate

    , signal, constant, track_signal, Method(..)
    , unpack

    , at, at_linear, sample

    , sig_add
    , sig_max, sig_min, clip_max, clip_min
    , truncate
    , map_x
) where
import Prelude hiding (truncate)
import qualified Control.Arrow as Arrow
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.StorableVector as V
import qualified Foreign.Storable as Storable

import qualified Util.Num as Num
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Track as Track

import qualified Perform.Pitch as Pitch
import qualified Perform.Timestamp as Timestamp
import qualified Perform.SignalBase as SignalBase
import Perform.SignalBase (Method(..), max_x, default_srate)
import qualified Perform.Signal as Signal


data PitchSignal = PitchSignal
    { sig_scale :: Pitch.ScaleId
    , sig_vec :: SignalBase.SigVec Y
    } deriving (Eq)

modify_vec :: (SignalBase.SigVec Y -> SignalBase.SigVec Y)
    -> PitchSignal -> PitchSignal
modify_vec f sig = sig { sig_vec = f (sig_vec sig) }

type RelativePitchSignal = Signal.Signal

type X = SignalBase.X
-- | Each sample is @(from, to, at)@, where @at@ is a normalized value between
-- 0 and 1 describing how far between @from@ and @to@ the value is.
--
-- This encoding consumes 12 bytes, but it seems like it should be possible to
-- reduce that.  There will be long sequences of samples with the same @from@
-- and @to@, and 'y_at' cares only about the first @from@ and the second @to@.
-- @(from, to, [(X, at)])@ is an obvious choice, but putting X inside the
-- sample will make the SignalBase stuff tricky... unless I have an extract_x
-- method.  Still, the list is variable length and StorableVector can't do
-- that.
type Y = (Float, Float, Float)
instance SignalBase.Signal Y

instance Storable.Storable (X, Y) where
    sizeOf _ = Storable.sizeOf (undefined :: Double)
        + Storable.sizeOf (undefined :: Float) * 3
    alignment _ = Storable.alignment (undefined :: Double)
    poke cp (pos, (from, to, at)) = do
        Storable.pokeByteOff cp 0 pos
        Storable.pokeByteOff cp 8 from
        Storable.pokeByteOff cp 12 to
        Storable.pokeByteOff cp 16 at
    peek cp = do
        pos <- Storable.peekByteOff cp 0 :: IO Double
        from <- Storable.peekByteOff cp 8 :: IO Float
        to <- Storable.peekByteOff cp 12 :: IO Float
        at <- Storable.peekByteOff cp 16 :: IO Float
        return (TrackPos pos, (from, to, at))

instance SignalBase.Y Y where
    zero_y = (0, 0, 0)
    y_at x0 (_, to0, at0) x1 (from1, _, at1) x = (to0, from1, at)
        where at = Num.scale at0 at1 (Num.normalize x0 x1 x)
    project y0 y1 at = (realToFrac y0, realToFrac y1, realToFrac at)

instance Show PitchSignal where
    show (PitchSignal scale_id vec) =
        "PitchSignal (" ++ show scale_id ++ ") " ++ show (V.unpack vec)

-- * construction / deconstruction

signal :: Pitch.ScaleId -> [(X, Y)] -> PitchSignal
signal scale_id ys = PitchSignal scale_id (V.pack ys)

constant :: Pitch.ScaleId -> Signal.Y -> PitchSignal
constant scale_id n = signal scale_id [(0, (realToFrac n, realToFrac n, 0))]

track_signal :: Pitch.ScaleId -> X -> [SignalBase.TrackSegment] -> PitchSignal
track_signal scale_id srate segs =
    PitchSignal scale_id (SignalBase.track_signal srate segs)

-- | Used for tests.
unpack :: PitchSignal -> [(X, Y)]
unpack = V.unpack . sig_vec

-- * access

at, at_linear :: X -> PitchSignal -> Y
at pos sig = SignalBase.at pos (sig_vec sig)
at_linear pos sig = SignalBase.at_linear pos (sig_vec sig)

sample :: X -> PitchSignal -> [(X, Y)]
sample start sig = SignalBase.sample start (sig_vec sig)


-- * functions

-- PitchSignal functions are asymmetric since they expect a PitchSignal and
-- a RelativePitchSignal.  If I want functions to combine absolute pitches from
-- possibly different scales I can write them later.

sig_add :: PitchSignal -> RelativePitchSignal -> PitchSignal
sig_add = sig_op $ \(from, to, at) y ->
    (from + realToFrac y, to + realToFrac y, at)

sig_max, sig_min :: PitchSignal -> RelativePitchSignal -> PitchSignal
sig_max = sig_op (flip ymin)
sig_min = sig_op (flip ymax)

clip_max, clip_min :: Pitch.NoteNumber -> PitchSignal -> PitchSignal
clip_max (Pitch.NoteNumber nn) = map_y (ymin nn)
clip_min (Pitch.NoteNumber nn) = map_y (ymax nn)

ymin, ymax :: Double -> Y -> Y
ymin val (from, to, at) = (from, to, at2)
    where at2 = Num.clamp 0 at (Num.normalize from to (realToFrac val))
ymax val (from, to, at) = (from, to, at2)
    where at2 = Num.clamp at 1 (Num.normalize from to (realToFrac val))

truncate :: X -> PitchSignal -> PitchSignal
truncate x = modify_vec (SignalBase.truncate x)

sig_op :: (Y -> Signal.Y -> Y)
    -> PitchSignal -> RelativePitchSignal -> PitchSignal
sig_op op (PitchSignal scale vec0) sig1 =
    PitchSignal scale (SignalBase.sig_op op vec0 (Signal.sig_vec sig1))

map_x :: (X -> X) -> PitchSignal -> PitchSignal
map_x f = modify_vec (SignalBase.map_x f)

map_y :: (Y -> Y) -> PitchSignal -> PitchSignal
map_y f = modify_vec (SignalBase.map_y f)
