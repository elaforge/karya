{-# LANGUAGE FlexibleInstances #-} -- for Error instance
{- | Convert from the Derive events to MIDI performer specific events.

    Since this module depends on both the Derive and Perform.Midi layers, it
    should be called from Derive or Cmd, not Perform.Midi, even though it's
    physically located in Perform.Midi.
-}
module Perform.Midi.Convert where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Strict as State

import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty

import qualified Midi.Midi as Midi
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Signal as Signal
import qualified Perform.Warning as Warning

import qualified Instrument.MidiDb as MidiDb
import Types


-- TODO record logs directly and remove warn_to_log

-- TODO warnings about:
-- - Instrument has a control that's not in its control map.

data Lookup = Lookup {
    lookup_scale :: Derive.LookupScale
    , lookup_inst :: MidiDb.LookupMidiInstrument
    , lookup_patch :: Score.Instrument -> Maybe Instrument.Patch
    }

-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Lookup -> Derive.Events -> [LEvent.LEvent Perform.Event]
convert lookup events = go Set.empty Nothing events
    where
    go _ _ [] = []
    go state prev (LEvent.Log log : rest) =
        LEvent.Log log : go state prev rest
    go state prev (LEvent.Event event : rest) =
        maybe [] ((:[]) . LEvent.Event) maybe_event ++ logs
            ++ go next_state (Just (Score.event_start event)) rest
        where
        (maybe_event, warns, next_state) = run_convert state
            (Score.event_stack event)
            (convert_event lookup prev event)
        logs = map (LEvent.Log . warn_to_log) warns

-- | Convert a Warning into an appropriate log msg.
warn_to_log :: Warning.Warning -> Log.Msg
warn_to_log (Warning.Warning msg stack maybe_range) =
    Log.msg Log.Warn (Just stack) $
        "Convert: " ++ msg ++ maybe "" ((" range: " ++) . show) maybe_range
    -- TODO It would be more useful to append the range to the stack, but
    -- I would have to convert real -> score.

convert_event :: Lookup -> Maybe RealTime -> Score.Event
    -> ConvertT Perform.Event
convert_event lookup maybe_prev event = do
    -- Sorted is a postcondition of the deriver.
    when_just maybe_prev $ \prev -> when (Score.event_start event < prev) $
        warn $ "start time less than previous of " ++ Pretty.pretty prev
    score_inst <- require "instrument" (Score.event_instrument event)
    (midi_inst, maybe_key) <- convert_inst (lookup_inst lookup) score_inst
        (Score.event_attributes event)
    patch <- require ("patch in instrument db: " ++ show score_inst) $
        lookup_patch lookup score_inst
    pitch <- case maybe_key of
        Nothing -> convert_pitch (Score.event_controls event)
            (Score.event_pitch event)
        Just key -> return $ Signal.constant (fromIntegral key)
    let controls = convert_controls
            (Instrument.has_flag Instrument.Pressure patch)
            (Instrument.inst_control_map midi_inst)
            (Score.event_controls event)
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
            warn $ "attrs have no match in keyswitches or keymap of "
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
    if Set.member inst not_found then Error.throwError Nothing
        else do
            State.put (Set.insert inst not_found)
            require ("midi instrument in instrument db: " ++ show inst
                ++ " (further warnings suppressed)") Nothing

-- | Convert deriver controls to performance controls.  Drop all non-MIDI
-- controls, since those will inhibit channel sharing later.
convert_controls :: Bool -- ^ True if the @p@ control should become breath.
    -> Control.ControlMap -- ^ Instrument's control map.
    -> Score.ControlMap -> Perform.ControlMap
convert_controls pressure inst_cmap =
    resolve_p .  Map.mapKeys cc . Map.map Score.typed_val
        . Map.filterWithKey (\k _ -> Control.is_midi_control inst_cmap k)
    where
    resolve_p cmap = case Map.lookup (cc Score.c_pressure) cmap of
        Nothing -> cmap
        Just sig -> Map.insert
            (if pressure then Control.c_breath else Control.c_velocity) sig
            cmap
    cc (Score.Control c) = Control.Control c

convert_pitch :: Score.ControlMap -> PitchSignal.Signal
    -> ConvertT Signal.NoteNumber
convert_pitch controls psig = do
    unless (null errs) $ warn $ "converting pitch: " ++ show errs
    return sig
    where
    (sig, errs) = PitchSignal.to_nn $ PitchSignal.apply_controls controls psig

-- * monad

-- | Bogus mandatory instance.
instance Error.Error (Maybe Warning.Warning) where
    strMsg = Just . Error.strMsg

-- | Remember which non-allocated instruments have been warned about to
-- suppress further warnings.
type State = Set.Set Score.Instrument

type ConvertT = Error.ErrorT (Maybe Warning.Warning)
    (State.StateT State
        (Logger.LoggerT Warning.Warning
            (Reader.ReaderT Stack.Stack Identity.Identity)))

warn :: String -> ConvertT ()
warn msg = do
    stack <- Reader.ask
    Logger.log (Warning.warning msg stack Nothing)

run_convert :: State -> Stack.Stack -> ConvertT a
    -> (Maybe a, [Warning.Warning], State)
run_convert state stack conv =
    (either (const Nothing) Just val, warn ++ warns, out_state)
    where
    run = Identity.runIdentity . flip Reader.runReaderT stack
        . Logger.run . flip State.runStateT state . Error.runErrorT
    ((val, out_state), warns) = run conv
    warn = case val of
        Left (Just warn) -> [warn]
        _ -> []

require :: String -> Maybe a -> ConvertT a
require msg val = do
    stack <- Reader.ask
    case val of
        Nothing -> Error.throwError $ Just $
            Warning.warning ("event requires " ++ msg) stack Nothing
        Just val -> return val
