-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Derive.PitchSignal (
    Signal, sig_scale_id
    , Scale(Scale), no_scale
    -- * construct and convert
    , constant, signal, unsignal, to_nn
    , unfoldr
    -- * apply controls
    , apply_controls, apply_control, controls_at
    -- * signal functions
    , null, at, shift, last
    , take, drop, drop_after, drop_before, drop_before_strict
    -- * Pitch
    , Pitch, pitch_scale_id, pitch_transposers
    , PitchError(..)
    , pitch, pitch_scale
    , apply, add_control, eval_pitch, eval_note, pitch_nn, pitch_note
) where
import Prelude hiding (take, drop, last, null)
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Data.Vector as V

import Util.Control
import qualified Util.Seq as Seq
import qualified Util.TimeVector as TimeVector

import qualified Derive.BaseTypes as Score
import Derive.BaseTypes
       (Signal(..), Pitch(..), Scale(..), ControlValMap, PitchError(..))
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
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
sig_scale = maybe no_scale (pitch_scale . TimeVector.sy)
    . TimeVector.head . sig_vec

modify_vector :: (TimeVector.Boxed Pitch -> TimeVector.Boxed Pitch)
    -> Signal -> Signal
modify_vector f sig = sig { sig_vec = f (sig_vec sig) }

no_scale :: Scale
no_scale = Scale (Pitch.ScaleId "no-scale") mempty

instance Monoid.Monoid Signal where
    mempty = Signal mempty
    mappend s1 s2 = Monoid.mconcat [s1, s2]
    mconcat [] = mempty
    mconcat sigs = Signal (TimeVector.merge (map sig_vec sigs))

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
    split ((x, pitch) : rest) = case pitch_nn pitch of
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
        apply
        (sample_controls controls (sig_transposers sig))
        (sig_vec sig)
    TimeVector.Sample start initial_pitch = V.unsafeHead (sig_vec sig)
    initial_controls = controls_at start controls

-- | Sample the ControlMap on the sample points of the given set of controls.
sample_controls :: ControlMap -> Set.Set Score.Control
    -> TimeVector.Boxed ControlValMap
sample_controls controls transposers =
    TimeVector.signal $ zip xs (map (flip controls_at controls) xs)
    where
    xs = Seq.drop_dups id $ Seq.merge_asc_lists id (map xs_of sigs)
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

controls_at :: RealTime -> ControlMap -> ControlValMap
controls_at t = Map.map (Signal.at t . Score.typed_val)

-- * signal functions

null :: Signal -> Bool
null = TimeVector.null . sig_vec

at :: RealTime -> Signal -> Maybe Pitch
at x = TimeVector.at x . sig_vec

shift :: RealTime -> Signal -> Signal
shift x = modify_vector (TimeVector.shift x)

last :: Signal -> Maybe (RealTime, Pitch)
last sig
    | V.null (sig_vec sig) = Nothing
    | otherwise = case V.unsafeLast (sig_vec sig) of
        TimeVector.Sample x pitch -> Just (x, pitch)

take :: Int -> Signal -> Signal
take = modify_vector . TimeVector.take

drop :: Int -> Signal -> Signal
drop = modify_vector . TimeVector.drop

drop_after :: RealTime -> Signal -> Signal
drop_after = modify_vector . TimeVector.drop_after

drop_before_strict :: RealTime -> Signal -> Signal
drop_before_strict = modify_vector . TimeVector.drop_before_strict

drop_before :: RealTime -> Signal -> Signal
drop_before = modify_vector . TimeVector.drop_before

-- * Pitch

pitch :: Scale
    -> (ControlValMap -> Either PitchError Pitch.NoteNumber)
    -> (ControlValMap -> Either PitchError Pitch.Note)
    -> Pitch
pitch scale nn note = Pitch
    { pitch_eval_nn = nn
    , pitch_eval_note = note
    , pitch_scale = scale
    }

pitch_scale_id :: Pitch -> Pitch.ScaleId
pitch_scale_id = pscale_scale_id . pitch_scale

pitch_transposers :: Pitch -> Set.Set Score.Control
pitch_transposers = pscale_transposers . pitch_scale

-- | Apply controls to a pitch.
apply :: ControlValMap -> Pitch -> Pitch
apply controls pitch = pitch
    { pitch_eval_nn = \controls2 ->
        pitch_eval_nn pitch $! Map.unionWith (+) controls2 controls
    , pitch_eval_note = \controls2 ->
        pitch_eval_note pitch $! Map.unionWith (+) controls2 controls
    }

add_control :: Score.Control -> Double -> Pitch -> Pitch
add_control control val pitch = pitch
    { pitch_eval_nn = \controls ->
        pitch_eval_nn pitch $! Map.insertWith (+) control val controls
    , pitch_eval_note = \controls ->
        pitch_eval_note pitch $! Map.insertWith (+) control val controls
    }

eval_pitch :: Pitch -> ControlValMap -> Either PitchError Pitch.NoteNumber
eval_pitch = pitch_eval_nn

eval_note :: Pitch -> ControlValMap -> Either PitchError Pitch.Note
eval_note = pitch_eval_note

pitch_nn :: Pitch -> Either PitchError Pitch.NoteNumber
pitch_nn p = eval_pitch p Map.empty

pitch_note :: Pitch -> Either PitchError Pitch.Note
pitch_note p = eval_note p Map.empty
