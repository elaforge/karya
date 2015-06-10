-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.PitchSignal (
    Signal, sig_scale_id
    , Scale(Scale), no_scale
    -- * construct and convert
    , constant, signal, unsignal, to_nn
    , unfoldr
    -- * apply controls
    , apply_controls, apply_control, apply_environ
    -- * signal functions
    , null, at, sample_at, before, shift, head, last
    , take, drop, drop_while, drop_after, drop_at_after
    , drop_before, drop_before_strict, drop_before_at, within
    , map_y
    , interleave, prepend
    , Sample(..)
    -- * Pitch
    , Transposed, Pitch
    , RawPitch, PitchConfig(..), pitch_scale_id, pitch_transposers
    , pitch_scale, pitch_eval_nn, pitch_eval_note, pitch_config, pitch_controls
    , PitchError(..)
    , pitch, coerce
    , config, apply, add_control, pitch_nn, pitch_note
) where
import Prelude hiding (head, take, drop, last, null)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import qualified Util.Seq as Seq
import qualified Util.TimeVector as TimeVector
import Util.TimeVector (Sample(..))

import qualified Derive.ScoreTypes as Score
import qualified Derive.BaseTypes as TrackLang
import Derive.BaseTypes
       (Signal(..), Transposed, Pitch, pitch, coerce, pitch_nn, pitch_note,
        RawPitch(..), Scale(..), PitchConfig(..), PitchError(..))

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global
import Types


-- Signal imported from BaseTypes.

-- | Set of transposers for the signal.  Transposers are documented in
-- 'pscale_transposers'.
--
-- A Signal can contain pitches from multiple scales, though I don't think this
-- should ever happen.  But if it does, the first pitch wins.
sig_transposers :: Signal -> Set.Set Score.Control
sig_transposers = pscale_transposers . sig_scale

-- | Get the scale id of the signal.
--
-- A Signal can contain pitches from multiple scales, though I don't think this
-- should ever happen.  But if it does, the first pitch wins.
sig_scale_id :: Signal -> Pitch.ScaleId
sig_scale_id = pscale_scale_id . sig_scale

sig_scale :: Signal -> Scale
sig_scale = maybe no_scale (pitch_scale . sy) . TimeVector.head . sig_vec

modify :: (TimeVector.Boxed Pitch -> TimeVector.Boxed Pitch)
    -> Signal -> Signal
modify f sig = sig { sig_vec = f (sig_vec sig) }

no_scale :: Scale
no_scale = Scale "no-scale" mempty

constant :: Pitch -> Signal
constant  = Signal . TimeVector.constant

signal :: [(RealTime, Pitch)] -> Signal
signal = Signal . TimeVector.signal

unsignal :: Signal -> [(RealTime, Pitch)]
unsignal = TimeVector.unsignal . sig_vec

-- | Flatten a signal to a non-transposeable Signal.NoteNumber.
to_nn :: Signal -> (Signal.NoteNumber, [PitchError])
to_nn sig = (Signal.signal nns, Set.toList errs)
    where
    (errs, nns) = split (unsignal sig)
    split [] = (Set.empty, [])
    split ((x, pitch) : rest) = case pitch_nn (coerce pitch) of
            -- TODO does this make a giant stack of thunks?
            Left err -> (Set.insert err errs, nns)
            Right (Pitch.NoteNumber nn) -> (errs, (x, nn) : nns)
        where (errs, nns) = split rest

unfoldr :: (state -> Maybe ((RealTime, Pitch), state)) -> state -> Signal
unfoldr f st = Signal $ TimeVector.unfoldr f st

type ControlMap = Map.Map Score.Control Score.TypedControl

-- | Resample the signal according to the 'sig_transposers' and apply the
-- given controls to the signal.
--
-- Controls are /added/ so if this is not correct for a given control then
-- this will do the wrong thing.  Transpose signals are probably mostly
-- additive so it'll be ok as long as you only apply transposing signals
-- and only apply the complete ControlMap once at the end (i.e.
-- "Perform.Midi.Convert").
apply_controls :: ControlMap -> Signal -> Signal
apply_controls controls sig
    | V.null (sig_vec sig) = sig
    | otherwise = sig { sig_vec = resampled }
    where
    resampled = TimeVector.sig_op2 initial_controls initial_pitch
        (\vmap -> coerce . apply vmap)
        (sample_controls controls (sig_transposers sig))
        (sig_vec sig)
    Sample start initial_pitch = V.unsafeHead (sig_vec sig)
    initial_controls = controls_at start controls

-- | Sample the ControlMap on the sample points of the given set of controls.
sample_controls :: ControlMap -> Set.Set Score.Control
    -> TimeVector.Boxed Score.ControlValMap
sample_controls controls transposers =
    TimeVector.signal $ zip xs (map (flip controls_at controls) xs)
    where
    xs = Seq.drop_dups id $ Seq.merge_lists id (map xs_of sigs)
    sigs = mapMaybe (\c -> Map.lookup c controls)
        (Set.toList transposers)
    xs_of = map fst . Signal.unsignal . Score.typed_val
    -- If the tsigs are dense, then it's wasteful to keep looking up all
    -- the values instead of stepping along in order, but if the tsigs are
    -- sparse then it's probably more efficient to sample.  I expect in many
    -- cases there will be 0 or 1 transposition values.

-- | 'apply_controls' specialized for a single control.
apply_control :: Score.Control -> Score.TypedControl -> Signal -> Signal
apply_control cont sig = apply_controls (Map.singleton cont sig)

-- | Apply an environ to all the pitches in the signal.  Unlike
-- 'apply_controls', this doesn't have to resample the signal.
apply_environ :: TrackLang.Environ -> Signal -> Signal
apply_environ env = modify $ TimeVector.map_y $ config (PitchConfig env mempty)

-- | Not exported, use the one in Derive.Score instead.
controls_at :: RealTime -> ControlMap -> Score.ControlValMap
controls_at t = Map.map (Signal.at t . Score.typed_val)

-- * signal functions

null :: Signal -> Bool
null = TimeVector.null . sig_vec

at :: RealTime -> Signal -> Maybe Pitch
at x = TimeVector.at x . sig_vec

sample_at :: RealTime -> Signal -> Maybe (RealTime, Pitch)
sample_at x = TimeVector.sample_at x . sig_vec

-- | Find the pitch immediately before the point.
before :: RealTime -> Signal -> Maybe Pitch
before x = fmap sy . TimeVector.before x . sig_vec

shift :: RealTime -> Signal -> Signal
shift x = modify (TimeVector.shift x)

head :: Signal -> Maybe (RealTime, Pitch)
head = fmap TimeVector.to_pair . TimeVector.head . sig_vec

last :: Signal -> Maybe (RealTime, Pitch)
last = fmap TimeVector.to_pair . TimeVector.last . sig_vec

take :: Int -> Signal -> Signal
take = modify . TimeVector.take

drop :: Int -> Signal -> Signal
drop = modify . TimeVector.drop

drop_while :: (Sample Pitch -> Bool) -> Signal -> Signal
drop_while f = modify (V.dropWhile f)

drop_after :: RealTime -> Signal -> Signal
drop_after = modify . TimeVector.drop_after

drop_at_after :: RealTime -> Signal -> Signal
drop_at_after = modify . TimeVector.drop_at_after

drop_before :: RealTime -> Signal -> Signal
drop_before = modify . TimeVector.drop_before

drop_before_strict :: RealTime -> Signal -> Signal
drop_before_strict = modify . TimeVector.drop_before_strict

drop_before_at :: RealTime -> Signal -> Signal
drop_before_at = modify . TimeVector.drop_before_at

within :: RealTime -> RealTime -> Signal -> Signal
within start end = modify $ TimeVector.within start end

map_y :: (Pitch -> Pitch) -> Signal -> Signal
map_y = modify . TimeVector.map_y

interleave :: Signal -> Signal -> Signal
interleave s1 s2 = Signal $ TimeVector.interleave (sig_vec s1) (sig_vec s2)

prepend :: Signal -> Signal -> Signal
prepend s1 s2 = Signal $ TimeVector.prepend (sig_vec s1) (sig_vec s2)

-- * Pitch

pitch_scale_id :: RawPitch a -> Pitch.ScaleId
pitch_scale_id = pscale_scale_id . pitch_scale

pitch_transposers :: Pitch -> Set.Set Score.Control
pitch_transposers = pscale_transposers . pitch_scale

pitch_controls :: PitchConfig -> Score.ControlValMap
pitch_controls (PitchConfig _ controls) = controls

-- | Apply a config to a pitch.
config :: PitchConfig -> RawPitch a -> RawPitch a
config c pitch = pitch { pitch_config = c <> pitch_config pitch }

-- | Apply just the controls part of a config to a pitch.
apply :: Score.ControlValMap -> Pitch -> Transposed
apply controls pitch = pitch { pitch_config = config <> pitch_config pitch }
    where config = PitchConfig mempty controls

add_control :: Score.Control -> Double -> RawPitch a -> RawPitch a
add_control control val pitch =
    pitch { pitch_config = config <> pitch_config pitch }
    where config = PitchConfig mempty (Map.singleton control val)
