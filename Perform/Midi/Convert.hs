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
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.ConvertUtil as ConvertUtil
import Perform.ConvertUtil (require)
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Instrument.MidiDb as MidiDb


-- | If true, warn if an instrument still has attributes left over after
-- selecting a keymap or keyswitch.  I'm using attributes rather freely, so
-- this can happen a lot, but it still seems like a useful check for debugging.
-- TODO come up with a better way to enable and disable this.
warn_unused_attributes :: Bool
warn_unused_attributes = False

type ConvertT a = ConvertUtil.ConvertT State a

-- | Remember which non-allocated instruments have been warned about to
-- suppress further warnings.
type State = Set.Set Score.Instrument

data Lookup = Lookup {
    lookup_scale :: Derive.LookupScale
    , lookup_inst :: MidiDb.LookupMidiInstrument
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
    patch <- get_patch score_inst $ lookup_patch lookup score_inst
    let (event, additional) =
            split_composite (Instrument.patch_composite patch) event_
    let attrs = Score.event_attributes event
    (midi_inst, ks_attrs) <- require
        ("instrument in db: " <> Pretty.pretty score_inst) $
            lookup_inst lookup attrs score_inst
    pitch <- convert_midi_pitch score_inst patch
        (Score.attrs_diff attrs ks_attrs) event
    let (controls, overridden) = convert_controls
            (Instrument.has_flag Instrument.Pressure patch)
            (Instrument.inst_control_map midi_inst)
            (Score.event_controls event
                `Map.union` lookup_default_controls lookup score_inst)
    when_just overridden $ \sig ->
        Log.warn $ "non-null control overridden by "
            ++ Pretty.pretty Score.c_dynamic ++ ": " ++ Pretty.pretty sig
    let converted = Perform.Event midi_inst
            (Score.event_start event) (Score.event_duration event)
            controls pitch (Score.event_stack event)
    return (converted, additional)

-- | Abort if the patch wasn't found.  If it wasn't found the first time, it
-- won't be found the second time either, so avoid spamming the log by throwing
-- Nothing after the first time.
get_patch :: Score.Instrument -> Maybe Instrument.Patch
    -> ConvertT Instrument.Patch
get_patch _ (Just v) = return v
get_patch inst Nothing = do
    not_found <- State.get
    if Set.member inst not_found then ConvertUtil.abort
        else do
            State.put (Set.insert inst not_found)
            require ("patch in instrument db: " ++ Pretty.pretty inst
                ++ " (further warnings suppressed)") Nothing

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
        , Score.event_controls =
            filter_key (`Set.notMember` split_controls)
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
convert_midi_pitch :: Score.Instrument -> Instrument.Patch -> Score.Attributes
    -> Score.Event -> ConvertT Signal.NoteNumber
convert_midi_pitch inst patch attrs event = do
    let kmap = Instrument.patch_keymap patch
    let maybe_key = Map.lookup attrs kmap
    when warn_unused_attributes $ case maybe_key of
        Nothing | attrs /= mempty ->
            Log.warn $ untxt $
                "attrs have no match in keyswitches or keymap of "
                <> ShowVal.show_val inst <> ": " <> ShowVal.show_val attrs
        -- If there was a keymap and lookup succeeded then all the attributes
        -- are accounted for.
        _ -> return ()
    case maybe_key of
        Nothing -> pitch_signal
        Just (low_key, _, Nothing) ->
            return $ Signal.constant (Midi.from_key low_key)
        Just (low_key, high_key, Just low_pitch) ->
            convert_keymap (Midi.from_key low_key) (Midi.from_key high_key)
                low_pitch =<< pitch_signal
    where
    convert_keymap :: Signal.Y -> Signal.Y -> Pitch.NoteNumber
        -> Signal.NoteNumber -> ConvertT Signal.NoteNumber
    convert_keymap low high low_pitch sig = return clipped
        where
        (clipped, out_of_range) = Signal.clip_bounds low high $
            Signal.scalar_add (low - un_nn low_pitch) sig
    un_nn (Pitch.NoteNumber nn) = nn
    pitch_signal = convert_pitch (Instrument.patch_scale patch)
        (Score.event_controls event) (Score.event_pitch event)

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convert_controls :: Bool -- ^ True if the @p@ control should become breath.
    -> Control.ControlMap -- ^ Instrument's control map.
    -> Score.ControlMap -- ^ Controls to convert.
    -> (Perform.ControlMap, Maybe (Score.Control, Score.TypedControl))
convert_controls pressure_inst inst_cmap =
    first (Map.fromAscList . map (\(k, v) -> (cc k, Score.typed_val v))
        . Map.toAscList
        . Map.filterWithKey (\k _ -> Control.is_midi_control inst_cmap k))
    . resolve_dyn
    where
    resolve_dyn cmap = case Map.lookup Score.c_dynamic cmap of
        Nothing -> (cmap, Nothing)
        Just sig -> insert_dyn sig cmap
    insert_dyn sig cmap = (Map.insert cont sig cmap, overridden)
        where
        cont = if pressure_inst then Score.c_breath else Score.c_velocity
        overridden = case Map.lookup cont cmap of
            Just sig | not (Signal.null (Score.typed_val sig))
                -> Just (cont, sig)
            _ -> Nothing
    cc (Score.Control c) = Control.Control c

convert_pitch :: Instrument.PatchScale -> Score.ControlMap
    -> PitchSignal.Signal -> ConvertT Signal.NoteNumber
convert_pitch scale controls psig = do
    unless (null errs) $ Log.warn $ "pitch: " ++ Pretty.pretty errs
    return $ convert_scale scale sig
    where
    (sig, errs) = PitchSignal.to_nn $ PitchSignal.apply_controls controls psig

convert_scale :: Instrument.PatchScale -> Signal.NoteNumber -> Signal.NoteNumber
convert_scale Nothing = id
convert_scale (Just scale) = Signal.map_y $
    un . Instrument.convert_patch_scale scale . Pitch.NoteNumber
    where un (Pitch.NoteNumber nn) = nn
