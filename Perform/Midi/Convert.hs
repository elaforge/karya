-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Convert from the Derive events to MIDI performer specific events.

    Since this module depends on both the Derive and Perform.Midi layers, it
    should be called from Derive or Cmd, not Perform.Midi, even though it's
    physically located in Perform.Midi.
-}
module Perform.Midi.Convert where
import qualified Control.Monad.State.Strict as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.TimeVector as TimeVector
import qualified Midi.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score

import qualified Perform.ConvertUtil as ConvertUtil
import Perform.ConvertUtil (require)
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Types as Types
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import Global


type ConvertT a = ConvertUtil.ConvertT State a

-- | Remember which non-allocated instruments have been warned about to
-- suppress further warnings.
type State = Set.Set Score.Instrument

data Lookup = Lookup {
    lookup_scale :: Derive.LookupScale
    , lookup_patch :: Score.Instrument
        -> Maybe (Patch.Patch, Score.Event -> Score.Event)
    , lookup_control_defaults :: Score.Instrument -> Score.ControlMap
    }

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Lookup -> [Score.Event] -> [LEvent.LEvent Types.Event]
convert lookup = ConvertUtil.convert Set.empty (convert_event lookup)

convert_event :: Lookup -> Score.Event -> ConvertT Types.Event
convert_event lookup event_ = do
    let score_inst = Score.event_instrument event_
    (patch, postproc) <- require_patch score_inst $
        lookup_patch lookup score_inst
    let midi_patch = Types.patch score_inst patch
    let event = postproc event_
    let event_controls = Score.event_transformed_controls event
    (midi_patch, pitch) <- convert_midi_pitch midi_patch
        (Patch.patch_scale patch) (Patch.patch_attribute_map patch)
        (Patch.has_flag patch Patch.ConstantPitch)
        event_controls event
    let controls = convert_controls (Types.patch_control_map midi_patch) $
            convert_dynamic pressure
                (event_controls <> lookup_control_defaults lookup score_inst)
        pressure = Patch.has_flag patch Patch.Pressure
        velocity = fromMaybe Perform.default_velocity
            (Env.maybe_val EnvKey.dynamic_val (Score.event_environ event))
        release_velocity = fromMaybe velocity
            (Env.maybe_val EnvKey.release_val (Score.event_environ event))
    return $ Types.Event
        { event_start = Score.event_start event
        , event_duration = Score.event_duration event
        , event_patch = midi_patch
        , event_controls = controls
        , event_pitch = pitch
        -- If it's a pressure instrument, then I'm using breath instead
        -- of velocity.  I still set velocity because some synths (e.g.
        -- vsl) use the velocity too in certain cases, but it should be
        -- at least 1 to avoid not even starting the note.  Of course
        -- even without pressure, a note on with velocity 0 is kind of
        -- pointless, but maybe someone wants to fade out.
        , event_start_velocity = if pressure then max 0.008 velocity
            else velocity
        , event_end_velocity = release_velocity
        , event_stack = Score.event_stack event
        }

-- | Abort if the patch wasn't found.  If it wasn't found the first time, it
-- won't be found the second time either, so avoid spamming the log by throwing
-- Nothing after the first time.
require_patch :: Score.Instrument -> Maybe a -> ConvertT a
require_patch _ (Just v) = return v
require_patch inst Nothing = do
    not_found <- State.get
    if Set.member inst not_found then ConvertUtil.abort
        else do
            State.put (Set.insert inst not_found)
            require ("patch in instrument db: " <> pretty inst
                <> " (further warnings suppressed)") Nothing

-- | If the Event has an attribute matching its keymap, use the pitch from the
-- keymap.  Otherwise convert the pitch signal.  Possibly warn if there are
-- attributes that didn't match anything.
--
-- TODO this used to warn about unmatched attributes, but it got annoying
-- because I use attributes freely.  It still seems like it could be useful,
-- so maybe I want to put it back in again someday.
convert_midi_pitch :: Types.Patch -> Maybe Patch.Scale
    -> Patch.AttributeMap -> Bool -> Score.ControlMap -> Score.Event
    -> ConvertT (Types.Patch, Signal.NoteNumber)
convert_midi_pitch patch scale attr_map constant_pitch controls event =
    case Common.lookup_attributes (Score.event_attributes event) attr_map of
        Nothing -> (,) patch <$> psignal
        Just (keyswitches, maybe_keymap) ->
            (,) (set_keyswitches keyswitches patch)
                <$> maybe psignal set_keymap maybe_keymap
    where
    set_keyswitches [] patch = patch
    set_keyswitches keyswitches patch =
        patch { Types.patch_keyswitch = keyswitches }
    set_keymap (Patch.UnpitchedKeymap key) =
        return $ Signal.constant (Midi.from_key key)
    set_keymap (Patch.PitchedKeymap low high low_nn) =
        convert_keymap (Midi.from_key low) (Midi.from_key high) low_nn
            =<< psignal
    convert_keymap :: Signal.Y -> Signal.Y -> Pitch.NoteNumber
        -> Signal.NoteNumber -> ConvertT Signal.NoteNumber
    convert_keymap low high low_pitch sig = return clipped
        where
        -- TODO warn about out_of_range
        (clipped, out_of_range) = Signal.clip_bounds low high $
            Signal.scalar_add (low - un_nn low_pitch) sig
    un_nn (Pitch.NoteNumber nn) = nn

    psignal
        | constant_pitch = convert cvals psig
        | otherwise = convert trimmed (Score.event_transformed_pitch event)
        where
        cvals = Score.untyped . Signal.constant <$>
            Score.event_controls_at (Score.event_start event) event
        psig = maybe mempty PSignal.constant $
            Score.pitch_at (Score.event_start event) event
        convert = convert_pitch scale (Score.event_environ event)
        -- Trim controls to avoid applying out of range transpositions.
        trimmed = fmap (fmap (Signal.drop_at_after note_end)) controls
        note_end = Score.event_end event
            + fromMaybe Types.default_decay (Types.patch_decay patch)

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convert_controls :: Control.ControlMap -- ^ Instrument's control map.
    -> Score.ControlMap -- ^ Controls to convert.
    -> Types.ControlMap
convert_controls inst_cmap =
    Map.fromAscList . map (second Score.typed_val) . Map.toAscList
        . Map.filterWithKey (\k _ -> Control.is_midi_control inst_cmap k)

-- | If it's a 'Patch.Pressure' instrument, move the 'Controls.dynamic'
-- control to 'Controls.breath'.
convert_dynamic :: Bool -> Score.ControlMap -> Score.ControlMap
convert_dynamic pressure controls
    | pressure = maybe controls
        (\sig -> Map.insert Controls.breath sig controls)
        (Map.lookup Controls.dynamic controls)
    | otherwise = controls

convert_pitch :: Maybe Patch.Scale -> Env.Environ
    -> Score.ControlMap -> PSignal.PSignal -> ConvertT Signal.NoteNumber
convert_pitch scale env controls psig = do
    let (sig, nn_errs) = PSignal.to_nn $ PSignal.apply_controls controls $
            PSignal.apply_environ env psig
    unless (null nn_errs) $ Log.warn $
        "convert pitch: " <> Text.intercalate ", " (map pretty nn_errs)
    let (nn_sig, scale_errs) = convert_scale scale sig
    unless (null scale_errs) $ Log.warn $
        "out of range for patch scale: " <> pretty scale_errs
    return $ Signal.map_y round_pitch nn_sig

-- | Round pitches to the nearest tenth of a cent.  Differences below this are
-- probably imperceptible.  Due to floating point inaccuracy, pitches can wind
-- up being slightly off of integral, leading to pitch bending where there
-- should be none.
round_pitch :: Signal.Y -> Signal.Y
round_pitch nn = fromIntegral (round (nn * 1000)) / 1000

convert_scale :: Maybe Patch.Scale -> Signal.NoteNumber
    -> (Signal.NoteNumber, [(Signal.X, Signal.Y)])
convert_scale Nothing = (, [])
convert_scale (Just scale) = Signal.map_err $ \(TimeVector.Sample x y) ->
    case Patch.convert_scale scale (Pitch.NoteNumber y) of
        Just (Pitch.NoteNumber nn) -> Right (TimeVector.Sample x nn)
        Nothing -> Left (x, y)
