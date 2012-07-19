{- | Convert from the Derive events to MIDI performer specific events.

    Since this module depends on both the Derive and Perform.Midi layers, it
    should be called from Derive or Cmd, not Perform.Midi, even though it's
    physically located in Perform.Midi.
-}
module Perform.Midi.Convert where
import qualified Control.Monad.State.Strict as State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score

import qualified Perform.ConvertUtil as ConvertUtil
import Perform.ConvertUtil (require)
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Instrument.MidiDb as MidiDb


type ConvertT a = ConvertUtil.ConvertT State a

-- | Remember which non-allocated instruments have been warned about to
-- suppress further warnings.
type State = Set.Set Score.Instrument

data Lookup = Lookup {
    lookup_scale :: Derive.LookupScale
    , lookup_inst :: MidiDb.LookupMidiInstrument
    , lookup_patch :: Score.Instrument -> Maybe Instrument.Patch
    }

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Lookup -> Derive.Events -> [LEvent.LEvent Perform.Event]
convert lookup = ConvertUtil.convert Set.empty (convert_event lookup)

convert_event :: Lookup -> Score.Event -> ConvertT Perform.Event
convert_event lookup event = do
    let score_inst = Score.event_instrument event
    (midi_inst, maybe_key) <- convert_inst (lookup_inst lookup) score_inst
        (Score.event_attributes event)
    patch <- require ("patch in instrument db: " ++ show score_inst) $
        lookup_patch lookup score_inst
    pitch <- case maybe_key of
        Nothing -> convert_pitch (Instrument.patch_scale patch)
            (Score.event_controls event) (Score.event_pitch event)
        Just key -> return $ Signal.constant (Midi.from_key key)
    let (controls, overridden) = convert_controls
            (Instrument.has_flag Instrument.Pressure patch)
            (Instrument.inst_control_map midi_inst)
            (Score.event_controls event)
    when_just overridden $ \sig ->
        Log.warn $ "non-null control overridden by "
            ++ Pretty.pretty Score.c_dynamic ++ ": " ++ Pretty.pretty sig
    return $ Perform.Event midi_inst
        (Score.event_start event) (Score.event_duration event)
        controls pitch (Score.event_stack event)

-- | Look up the score inst and figure out keyswitches and keymap based on
-- its attributes.  Warn if there are attributes that didn't match anything.
convert_inst :: MidiDb.LookupMidiInstrument -> Score.Instrument
    -> Score.Attributes -> ConvertT (Instrument.Instrument, Maybe Midi.Key)
convert_inst lookup_inst score_inst attrs = do
    (midi_inst, ks_attrs) <- get_inst score_inst (lookup_inst attrs score_inst)
    let kmap_attrs = Score.attrs_diff attrs ks_attrs
    let kmap = Instrument.inst_keymap midi_inst
    maybe_key <- if Map.null kmap
        then return Nothing
        else case Map.lookup kmap_attrs kmap of
            Nothing -> return Nothing
            Just key -> return (Just key)
    case maybe_key of
        Nothing | kmap_attrs /= Score.no_attrs ->
            Log.warn $ "attrs have no match in keyswitches or keymap of "
                ++ Pretty.pretty midi_inst ++ ": " ++ Pretty.pretty kmap_attrs
        -- If there was a keymap and lookup succeeded then all the attributes
        -- are accounted for.
        _ -> return ()
    return (midi_inst, maybe_key)

-- | Lookup the instrument or throw.  If an instrument wasn't found the first
-- time, it won't be found the second time either, so avoid spamming the log
-- by throwing Nothing after the first time.
get_inst :: Score.Instrument -> Maybe (Instrument.Instrument, Score.Attributes)
    -> ConvertT (Instrument.Instrument, Score.Attributes)
get_inst _ (Just v) = return v
get_inst inst Nothing = do
    not_found <- State.get
    if Set.member inst not_found then ConvertUtil.abort
        else do
            State.put (Set.insert inst not_found)
            require ("midi instrument in instrument db: " ++ Pretty.pretty inst
                ++ " (further warnings suppressed)") Nothing

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convert_controls :: Bool -- ^ True if the @p@ control should become breath.
    -> Control.ControlMap -- ^ Instrument's control map.
    -> Score.ControlMap
    -> (Perform.ControlMap, Maybe (Score.Control, Score.TypedSignal))
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
