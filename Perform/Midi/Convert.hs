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
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.ConvertUtil as ConvertUtil
import Perform.ConvertUtil (require)
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import Global


type ConvertT a = ConvertUtil.ConvertT State a

-- | Remember which non-allocated instruments have been warned about to
-- suppress further warnings.
type State = Set.Set Score.Instrument

data Lookup = Lookup {
    lookup_scale :: Derive.LookupScale
    , lookup_inst :: Score.Instrument -> Maybe Instrument.Instrument
    , lookup_patch :: Score.Instrument
        -> Maybe (Instrument.Patch, Score.Event -> Score.Event)
    , lookup_default_controls :: Score.Instrument -> Score.ControlMap
    }

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Lookup -> [Score.Event] -> [LEvent.LEvent Perform.Event]
convert lookup = ConvertUtil.convert Set.empty (convert_event lookup)

convert_event :: Lookup -> Score.Event -> ConvertT Perform.Event
convert_event lookup event_ = do
    let score_inst = Score.event_instrument event_
    (patch, postproc) <- require_patch score_inst $
        lookup_patch lookup score_inst
    let event = postproc event_
    midi_inst <- require ("instrument not in db: " <> prettyt score_inst) $
        lookup_inst lookup score_inst
    let event_controls = Score.event_transformed_controls event
    (midi_inst, pitch) <- convert_midi_pitch midi_inst
        (Instrument.patch_scale patch) (Instrument.patch_attribute_map patch)
        (Instrument.has_flag Instrument.ConstantPitch patch)
        event_controls event
    let (controls, overridden) =
            first (convert_controls (Instrument.inst_control_map midi_inst)) $
            convert_dynamic (Instrument.has_flag Instrument.Pressure patch)
                (event_controls
                    `Map.union` lookup_default_controls lookup score_inst)
                (TrackLang.maybe_val Environ.dynamic_val
                    (Score.event_environ event))
    whenJust overridden $ \sig ->
        Log.warn $ "non-null control overridden by "
            <> prettyt Controls.dynamic <> ": " <> prettyt sig
    let converted = Perform.Event
            { Perform.event_instrument = midi_inst
            , Perform.event_start = Score.event_start event
            , Perform.event_duration = Score.event_duration event
            , Perform.event_controls = controls
            , Perform.event_pitch = pitch
            , Perform.event_stack = Score.event_stack event
            }
    return converted

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
            require ("patch in instrument db: " <> prettyt inst
                <> " (further warnings suppressed)") Nothing

-- | If the Event has an attribute matching its keymap, use the pitch from the
-- keymap.  Otherwise convert the pitch signal.  Possibly warn if there are
-- attributes that didn't match anything.
--
-- TODO this used to warn about unmatched attributes, but it got annoying
-- because I use attributes freely.  It still seems like it could be useful,
-- so maybe I want to put it back in again someday.
convert_midi_pitch :: Instrument.Instrument -> Maybe Instrument.PatchScale
    -> Instrument.AttributeMap -> Bool -> Score.ControlMap -> Score.Event
    -> ConvertT (Instrument.Instrument, Signal.NoteNumber)
convert_midi_pitch inst patch_scale attr_map constant_pitch
        controls event =
    case Instrument.lookup_attribute (Score.event_attributes event) attr_map of
        Nothing -> (,) inst <$> pitch_signal
        Just (keyswitches, maybe_keymap) ->
            (,) (set_keyswitches keyswitches inst)
                <$> maybe pitch_signal set_keymap maybe_keymap
    where
    set_keyswitches [] inst = inst
    set_keyswitches keyswitches inst = inst
        { Instrument.inst_keyswitch = keyswitches }
    set_keymap (Instrument.UnpitchedKeymap key) =
        return $ Signal.constant (Midi.from_key key)
    set_keymap (Instrument.PitchedKeymap low high low_nn) =
        convert_keymap (Midi.from_key low) (Midi.from_key high) low_nn
            =<< pitch_signal
    convert_keymap :: Signal.Y -> Signal.Y -> Pitch.NoteNumber
        -> Signal.NoteNumber -> ConvertT Signal.NoteNumber
    convert_keymap low high low_pitch sig = return clipped
        where
        -- TODO warn about out_of_range
        (clipped, out_of_range) = Signal.clip_bounds low high $
            Signal.scalar_add (low - un_nn low_pitch) sig
    un_nn (Pitch.NoteNumber nn) = nn

    pitch_signal
        | constant_pitch = convert cvals psig
        | otherwise = convert trimmed (Score.event_transformed_pitch event)
        where
        cvals = fmap (Score.untyped . Signal.constant) $
            Score.event_controls_at (Score.event_start event) event
        psig = maybe mempty PitchSignal.constant $
            Score.pitch_at (Score.event_start event) event
        convert = convert_pitch patch_scale
        -- Trim controls to avoid applying out of range transpositions.
        trimmed = fmap (fmap (Signal.drop_at_after note_end)) controls
        note_end = Score.event_end event + Instrument.inst_decay inst

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convert_controls :: Control.ControlMap -- ^ Instrument's control map.
    -> Score.ControlMap -- ^ Controls to convert.
    -> Perform.ControlMap
convert_controls inst_cmap =
    Map.fromAscList . map (second Score.typed_val) . Map.toAscList
        . Map.filterWithKey (\k _ -> Control.is_midi_control inst_cmap k)

{- | I originally intended to move this to the note call, and replace the
    'Instrument.Pressure' with a note call override.  But it turns out that
    Pressure is also used by Cmd, so I still need it.  But to get vel from
    control functions I have to include the ControlValMap in Score.Event.

    I waffled for a long time about whether it was better to handle in the note
    call or in conversion, and initially favored the note call because it feels
    like complexity should go in Derive, which is configurable, and not in
    Convert.  But it turns out doing dyn mapping in Derive then breaks
    integration, so I'd have to undo it for integration.  That made me think
    that this is really a low level MIDI detail and perhaps it's best handled
    by Convert after all.

    A side effect is that NoteOff velocities are now always the same as NoteOn
    ones, since velocity is now a constant sample.  I can revisit this if
    I ever care about NoteOff velocity.

    One problem with doing the dyn conversion here is that for a control
    function on dyn to have any effect I need the value from the control
    function, which means I need a scalar value.  But by the time I get here
    the control functions are already gone.  The note call can't know
    which of the dyn signal or control function is wanted, because that
    decision is made here.  One solution was to put a ControlValMap in
    Score.Event so they get here, but that means that anything that modifies
    controls also has to remember to modify the ControlValMap.  I can do that
    by updating 'Score.modify_control', but it seems like overkill when all
    I really want is to communicate the dyn value.  So instead I stash the
    control function value in 'Controls.dynamic_function'.  Unfortunately this
    brings it's own complications since now I need to remember to modify it
    when I modify an event's dynamic, and filter it out of integration so it
    doesn't create a track for it.

    So neither way is very satisfying, but at least this way doesn't require
    a whole new field in Score.Event.  Perhaps I'll come up with something
    better someday.
-}
convert_dynamic :: Bool -- ^ True if the @p@ control should become breath.
    -> Score.ControlMap -- ^ Controls to convert.
    -> Maybe Signal.Y
    -> (Score.ControlMap, Maybe Score.Control)
convert_dynamic pressure controls dyn_function =
    maybe (controls, Nothing) insert_dyn dyn
    where
    dyn = if pressure then Map.lookup Controls.dynamic controls
        else Score.untyped . Signal.constant <$> dyn_function
    dest = if pressure then Controls.breath else Controls.velocity
    insert_dyn sig = (Map.insert dest sig controls, overridden)
    overridden = case Map.lookup dest controls of
        Just sig | not (Signal.null (Score.typed_val sig))
            -> Just dest
        _ -> Nothing

convert_pitch :: Maybe Instrument.PatchScale -> Score.ControlMap
    -> PitchSignal.Signal -> ConvertT Signal.NoteNumber
convert_pitch scale controls psig = do
    let (sig, nn_errs) = PitchSignal.to_nn $
            PitchSignal.apply_controls controls psig
    unless (null nn_errs) $ Log.warn $
        "convert pitch: " <> Text.intercalate ", " (map prettyt nn_errs)
    let (nn_sig, scale_errs) = convert_scale scale sig
    unless (null scale_errs) $ Log.warn $
        "out of range for patch scale: " <> prettyt scale_errs
    return $ Signal.map_y round_pitch nn_sig

-- | Round pitches to the nearest tenth of a cent.  Differences below this are
-- probably imperceptible.  Due to floating point inaccuracy, pitches can wind
-- up being slightly off of integral, leading to pitch bending where there
-- should be none.
round_pitch :: Signal.Y -> Signal.Y
round_pitch nn = fromIntegral (round (nn * 1000)) / 1000

convert_scale :: Maybe Instrument.PatchScale -> Signal.NoteNumber
    -> (Signal.NoteNumber, [(Signal.X, Signal.Y)])
convert_scale Nothing = flip (,) []
convert_scale (Just scale) = Signal.map_err $ \(TimeVector.Sample x y) ->
    case Instrument.convert_patch_scale scale (Pitch.NoteNumber y) of
        Just (Pitch.NoteNumber nn) -> Right (TimeVector.Sample x nn)
        Nothing -> Left (x, y)
