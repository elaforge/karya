-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.PSignal (
    PSignal, sig_scale_id
    , Scale(Scale), no_scale
    -- * construct and convert
    , constant, signal, unsignal, unsignal_unique, set, to_nn
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
    , RawPitch, PitchConfig(..)
    , symbolic_pitch
    , pitch_scale_id, pitch_transposers
    , pitch_scale, pitch_eval_nn, pitch_eval_note, pitch_config, pitch_controls
    , PitchError(..)
    , pitch, coerce
    , config, apply, add_control, pitch_nn, pitch_note
    -- ** create
    , nn_pitch, note_pitch
) where
import Prelude hiding (head, take, drop, last, null)
import qualified Data.Either as Either
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

import qualified Util.Seq as Seq
import qualified Util.TimeVector as TimeVector
import Util.TimeVector (Sample(..))

import qualified Derive.BaseTypes as BaseTypes
import Derive.BaseTypes
       (PSignal(..), Transposed, Pitch, pitch, coerce, pitch_nn, pitch_note,
        RawPitch(..), Scale(..), PitchConfig(..), PitchError(..))
import qualified Derive.ScoreTypes as Score

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
sig_transposers :: PSignal -> Set Score.Control
sig_transposers = pscale_transposers . sig_scale

-- | Get the scale id of the signal.
--
-- A PSignal can contain pitches from multiple scales, though I don't think this
-- should ever happen.  But if it does, the first pitch wins.
sig_scale_id :: PSignal -> Pitch.ScaleId
sig_scale_id = pscale_scale_id . sig_scale

sig_scale :: PSignal -> Scale
sig_scale = maybe no_scale (pitch_scale . sy) . TimeVector.head . sig_vec

modify :: (TimeVector.Boxed Pitch -> TimeVector.Boxed Pitch)
    -> PSignal -> PSignal
modify f sig = sig { sig_vec = f (sig_vec sig) }

no_scale :: Scale
no_scale = Scale "no-scale" mempty

constant :: Pitch -> PSignal
constant  = PSignal . TimeVector.constant

signal :: [(RealTime, Pitch)] -> PSignal
signal = PSignal . TimeVector.signal

unsignal :: PSignal -> [(RealTime, Pitch)]
unsignal = TimeVector.unsignal . sig_vec

unsignal_unique :: PSignal -> [(RealTime, Pitch)]
unsignal_unique = TimeVector.unsignal_unique . sig_vec

-- | Set the signal value, with a discontinuity.
set :: Maybe Pitch -> RealTime -> Pitch -> PSignal
set prev_y x y = PSignal $ TimeVector.set prev_y x y

-- | Flatten a signal to a non-transposeable Signal.NoteNumber.
to_nn :: PSignal -> (Signal.NoteNumber, [PitchError])
to_nn = extract . Either.partitionEithers . map eval . unsignal
    where
    extract (errs, nns) = (Signal.signal nns, Seq.unique_sort errs)
    eval (x, pitch) = case pitch_nn (coerce pitch) of
        Left err -> Left err
        Right (Pitch.NoteNumber nn) -> Right (x, nn)

unfoldr :: (state -> Maybe ((RealTime, Pitch), state)) -> state -> PSignal
unfoldr f st = PSignal $ TimeVector.unfoldr f st

type ControlMap = Map Score.Control Score.TypedControl

-- | Resample the signal according to the 'sig_transposers' and apply the
-- given controls to the signal.
--
-- Controls are /added/ so if this is not correct for a given control then
-- this will do the wrong thing.  Transpose signals are probably mostly
-- additive so it'll be ok as long as you only apply transposing signals
-- and only apply the complete ControlMap once at the end (i.e.
-- "Perform.Midi.Convert").
apply_controls :: ControlMap -> PSignal -> PSignal
apply_controls controls sig
    | Just (x, _) <- head sig = sig { sig_vec = resample x }
    | otherwise = sig
    where
    resample x = TimeVector.sig_op_poly initial_controls initial_pitch
        (\vmap -> coerce . apply vmap)
        (sample_controls x (trim x controls) (sig_transposers sig))
        (sig_vec sig)
    trim = fmap . fmap . Signal.drop_before
    Sample start initial_pitch = V.unsafeHead (sig_vec sig)
    initial_controls = controls_at start controls

-- | Sample the ControlMap on the sample points of the given set of controls.
sample_controls :: RealTime -> ControlMap -> Set Score.Control
    -> TimeVector.Boxed Score.ControlValMap
sample_controls start controls transposers =
    TimeVector.signal $ zip xs (map (flip controls_at controls) xs)
    where
    xs = Seq.drop_dups id $ Seq.merge_lists id (map xs_of sigs)
    sigs = mapMaybe (\c -> Map.lookup c controls) (Set.toList transposers)
    -- dropWhile (<start) because the xs may start before the start time to
    -- get initial values, but I don't want to extend the pitch signal to
    -- before where it originally started.  This would cause a problem when
    -- flattening a PSignal for the track signal, where a transpose signal
    -- could cause every pitch signal fragment to start at 0.
    xs_of = dropWhile (<start) . map fst . Signal.unsignal . Score.typed_val
    -- If the tsigs are dense, then it's wasteful to keep looking up all
    -- the values instead of stepping along in order, but if the tsigs are
    -- sparse then it's probably more efficient to sample.  I expect in many
    -- cases there will be 0 or 1 transposition values.

-- | 'apply_controls' specialized for a single control.
apply_control :: Score.Control -> Score.TypedControl -> PSignal -> PSignal
apply_control cont sig = apply_controls (Map.singleton cont sig)

-- | Apply an environ to all the pitches in the signal.  Unlike
-- 'apply_controls', this doesn't have to resample the signal.
apply_environ :: BaseTypes.Environ -> PSignal -> PSignal
apply_environ env = modify $ TimeVector.map_y $ config (PitchConfig env mempty)

-- | Not exported, use the one in Derive.Score instead.
controls_at :: RealTime -> ControlMap -> Score.ControlValMap
controls_at t = Map.map (Signal.at t . Score.typed_val)

-- * signal functions

null :: PSignal -> Bool
null = TimeVector.null . sig_vec

at :: RealTime -> PSignal -> Maybe Pitch
at x = TimeVector.at x . sig_vec

sample_at :: RealTime -> PSignal -> Maybe (RealTime, Pitch)
sample_at x = TimeVector.sample_at x . sig_vec

-- | Find the last pitch before the point.
before :: RealTime -> PSignal -> Maybe (RealTime, Pitch)
before x = fmap TimeVector.to_pair . TimeVector.before x . sig_vec

shift :: RealTime -> PSignal -> PSignal
shift x = modify (TimeVector.shift x)

head :: PSignal -> Maybe (RealTime, Pitch)
head = fmap TimeVector.to_pair . TimeVector.head . sig_vec

last :: PSignal -> Maybe (RealTime, Pitch)
last = fmap TimeVector.to_pair . TimeVector.last . sig_vec

take :: Int -> PSignal -> PSignal
take = modify . TimeVector.take

drop :: Int -> PSignal -> PSignal
drop = modify . TimeVector.drop

drop_while :: (Sample Pitch -> Bool) -> PSignal -> PSignal
drop_while f = modify (V.dropWhile f)

drop_after :: RealTime -> PSignal -> PSignal
drop_after = modify . TimeVector.drop_after

drop_at_after :: RealTime -> PSignal -> PSignal
drop_at_after = modify . TimeVector.drop_at_after

drop_before :: RealTime -> PSignal -> PSignal
drop_before = modify . TimeVector.drop_before

drop_before_strict :: RealTime -> PSignal -> PSignal
drop_before_strict = modify . TimeVector.drop_before_strict

drop_before_at :: RealTime -> PSignal -> PSignal
drop_before_at = modify . TimeVector.drop_before_at

within :: RealTime -> RealTime -> PSignal -> PSignal
within start end = modify $ TimeVector.within start end

map_y :: (Pitch -> Pitch) -> PSignal -> PSignal
map_y = modify . TimeVector.map_y

interleave :: PSignal -> PSignal -> PSignal
interleave s1 s2 = PSignal $ TimeVector.interleave (sig_vec s1) (sig_vec s2)

prepend :: PSignal -> PSignal -> PSignal
prepend s1 s2 = PSignal $ TimeVector.prepend (sig_vec s1) (sig_vec s2)

-- * Pitch

-- | This is like pretty for pitch, but just shows the symbolic note name.
symbolic_pitch :: RawPitch a -> Text
symbolic_pitch = either showt Pitch.note_text . pitch_note . coerce

pitch_scale_id :: RawPitch a -> Pitch.ScaleId
pitch_scale_id = pscale_scale_id . pitch_scale

pitch_transposers :: Pitch -> Set Score.Control
pitch_transposers = pscale_transposers . pitch_scale

pitch_controls :: PitchConfig -> Score.ControlValMap
pitch_controls (PitchConfig _ controls) = controls

-- | Apply a config to a pitch.
config :: PitchConfig -> RawPitch a -> RawPitch b
config c pitch = pitch { pitch_config = c <> pitch_config pitch }

-- | Apply just the controls part of a config to a pitch.
apply :: Score.ControlValMap -> Pitch -> Transposed
apply controls = config (PitchConfig mempty controls)

add_control :: Score.Control -> Double -> RawPitch a -> RawPitch a
add_control control val pitch =
    pitch { pitch_config = config <> pitch_config pitch }
    where config = PitchConfig mempty (Map.singleton control val)

-- ** create

-- | Create a Pitch that only emits the given NoteNumber, and doesn't respond
-- to transposition.
nn_pitch :: Pitch.NoteNumber -> Pitch
nn_pitch nn = note_pitch (Pitch.Note (pretty nn)) nn

note_pitch :: Pitch.Note -> Pitch.NoteNumber -> Pitch
note_pitch note nn =
    pitch no_scale (const (Right nn)) (const $ Right note) mempty
