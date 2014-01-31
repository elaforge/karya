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
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.TimeVector as TimeVector

import qualified Midi.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
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


type ConvertT a = ConvertUtil.ConvertT State a

-- | Remember which non-allocated instruments have been warned about to
-- suppress further warnings.
type State = Set.Set Score.Instrument

data Lookup = Lookup {
    lookup_scale :: Derive.LookupScale
    , lookup_inst :: Score.Instrument -> Maybe Instrument.Instrument
    , lookup_patch :: Score.Instrument -> Maybe Instrument.Patch
    , lookup_default_controls :: Score.Instrument -> Score.ControlMap
    }

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Lookup -> [Score.Event] -> [LEvent.LEvent Perform.Event]
convert lookup = ConvertUtil.convert Set.empty (convert_event lookup)

convert_event :: Lookup -> Score.Event
    -> ConvertT (Perform.Event, [Score.Event])
convert_event lookup event_ = do
    let score_inst = Score.event_instrument event_
    patch <- require_patch score_inst $ lookup_patch lookup score_inst
    let (event, additional) =
            split_composite (Instrument.patch_composite patch) event_
    midi_inst <- require ("instrument in db: " <> pretty score_inst) $
        lookup_inst lookup score_inst
    (midi_inst, pitch) <- convert_midi_pitch midi_inst
        (Instrument.patch_environ patch) (Instrument.patch_scale patch)
        (Instrument.patch_attribute_map patch) event
    let (controls, overridden) =
            first (convert_controls (Instrument.inst_control_map midi_inst)) $
            convert_dynamic (Instrument.has_flag Instrument.Pressure patch)
                (Score.event_controls event
                    `Map.union` lookup_default_controls lookup score_inst)
    whenJust overridden $ \sig ->
        Log.warn $ "non-null control overridden by "
            <> pretty Controls.dynamic <> ": " <> pretty sig
    let converted = Perform.Event
            { Perform.event_instrument = midi_inst
            , Perform.event_start = Score.event_start event
            , Perform.event_duration = Score.event_duration event
            , Perform.event_controls = controls
            , Perform.event_pitch = pitch
            , Perform.event_stack = Score.event_stack event
            }
    return (converted, additional)

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

-- | Split a composite patch, as documented in 'Instrument.Composite'.
split_composite :: [Instrument.Composite] -> Score.Event
    -> (Score.Event, [Score.Event])
split_composite [] event = (event, [])
split_composite composite event = (stripped, map extract composite)
    where
    stripped = event
        { Score.event_pitch =
            if any Maybe.isNothing [p | (_, p, _) <- composite]
                then mempty else Score.event_pitch event
        , Score.event_controls = filter_key (`Set.notMember` split_controls)
        }
    split_controls = mconcat [cs | (_, _, cs) <- composite]
    extract (inst, maybe_pitch, controls) = event
        { Score.event_instrument = inst
        , Score.event_pitch = case maybe_pitch of
            Nothing -> Score.event_pitch event
            Just c -> Map.findWithDefault mempty c (Score.event_pitches event)
        , Score.event_controls = filter_key (`Set.member` controls)
        }
    filter_key f = Map.filterWithKey (\k _ -> f k) (Score.event_controls event)

-- | If the Event has an attribute matching its keymap, use the pitch from the
-- keymap.  Otherwise convert the pitch signal.  Possibly warn if there are
-- attributes that didn't match anything.
--
-- TODO this used to warn about unmatched attributes, but it got annoying
-- because I use attributes freely.  It still seems like it could be useful,
-- so maybe I want to put it back in again someday.
convert_midi_pitch :: Instrument.Instrument -> TrackLang.Environ
    -- ^ The environ that the instrument itself implies.
    -> Instrument.PatchScale
    -> Instrument.AttributeMap -> Score.Event
    -> ConvertT (Instrument.Instrument, Signal.NoteNumber)
convert_midi_pitch inst inst_environ patch_scale attr_map event =
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
    pitch_signal = convert_pitch patch_scale
        (inst_environ <> Score.event_environ event)
        (Score.event_controls event) (Score.event_pitch event)

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
    -> (Score.ControlMap, Maybe Score.Control)
convert_dynamic pressure controls =
    maybe (controls, Nothing) insert_dyn (Map.lookup source controls)
    where
    dest = if pressure then Controls.breath else Controls.velocity
    source = if pressure then Controls.dynamic else Controls.dynamic_function
    insert_dyn sig = (Map.insert dest sig controls, overridden)
    overridden = case Map.lookup dest controls of
        Just sig | not (Signal.null (Score.typed_val sig))
            -> Just dest
        _ -> Nothing

convert_pitch :: Instrument.PatchScale -> TrackLang.Environ -> Score.ControlMap
    -> PitchSignal.Signal -> ConvertT Signal.NoteNumber
convert_pitch scale environ controls psig = do
    unless (null errs) $ Log.warn $ "pitch: " <> pretty errs
    let (nn_sig, errs) = convert_scale scale (Signal.map_y round_pitch sig)
    unless (null errs) $
        Log.warn $ "out of range for patch scale: " <> pretty errs
    return nn_sig
    where
    (sig, errs) = PitchSignal.to_nn $
        PitchSignal.apply_controls environ controls psig

-- | Round pitches to the nearest tenth of a cent.  Differences below this are
-- probably imperceptible.  Due to floating point inaccuracy, pitches can wind
-- up being slightly off of integral, leading to pitch bending where there
-- should be none.
round_pitch :: Signal.Y -> Signal.Y
round_pitch nn = fromIntegral (round (nn * 1000)) / 1000

convert_scale :: Instrument.PatchScale -> Signal.NoteNumber
    -> (Signal.NoteNumber, [(Signal.X, Signal.Y)])
convert_scale Nothing = flip (,) []
convert_scale (Just scale) = Signal.map_err $ \(TimeVector.Sample x y) ->
    case Instrument.convert_patch_scale scale (Pitch.NoteNumber y) of
        Just (Pitch.NoteNumber nn) -> Right (TimeVector.Sample x nn)
        Nothing -> Left (x, y)
