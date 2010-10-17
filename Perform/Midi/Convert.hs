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
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Midi.Midi as Midi

import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty

import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb

-- TODO warnings about:
-- - Instrument has a control that's not in its control map.
-- - No allocation should be warned about in performer?


-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: Derive.LookupScale -> MidiDb.LookupMidiInstrument -> [Score.Event]
    -> ([Perform.Event], [Warning.Warning])
convert lookup_scale lookup_inst events = (maybe [] id evts, warns)
    where
    (evts, warns) = run_convert $ fmap Maybe.catMaybes $ mapM conv_catch events
    conv_catch event = fmap Just (conv_event event) `Error.catchError` catch
    conv_event event =
        Reader.local (const (Score.event_stack event))
            (convert_event lookup_scale lookup_inst event)
    catch Nothing = return Nothing
    catch (Just warn) = Logger.log warn >> return Nothing

convert_event :: Derive.LookupScale -> MidiDb.LookupMidiInstrument
    -> Score.Event -> ConvertT Perform.Event
convert_event lookup_scale lookup_inst event = do
    score_inst <- require "instrument" (Score.event_instrument event)
    (midi_inst, maybe_key) <- convert_inst lookup_inst score_inst
        (Score.event_attributes event)
    pitch <- case maybe_key of
        Nothing -> convert_pitch lookup_scale (Score.event_pitch event)
        Just key -> return $ Signal.constant (fromIntegral key)
    let controls = convert_controls (Score.event_controls event)
    return $ Perform.Event midi_inst
        (Timestamp.from_real_time (Score.event_start event))
        (Timestamp.from_real_time (Score.event_duration event))
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

-- | They're both newtypes so this should boil down to id.
-- I could filter out the ones MIDI doesn't handle but laziness should do its
-- thing.  TODO unless that prevents timely GC?
convert_controls :: Score.ControlMap -> Perform.ControlMap
convert_controls = Map.mapKeys (\(Score.Control c) -> Control.Control c)

convert_pitch :: Derive.LookupScale -> PitchSignal.PitchSignal
    -> ConvertT Signal.NoteNumber
convert_pitch lookup_scale psig = case lookup_scale scale_id of
    Nothing -> do
        warn $ "unknown scale: " ++ show scale_id
        return (Signal.constant Signal.invalid_pitch)
    Just scale -> return $ PitchSignal.to_nn (Scale.degree_to_double scale) psig
    where scale_id = PitchSignal.sig_scale psig

-- * monad

-- | Bogus mandatory instance.
instance Error.Error (Maybe Warning.Warning) where
    strMsg = Just . Error.strMsg

type ConvertT = Error.ErrorT (Maybe Warning.Warning)
    (State.StateT (Set.Set Score.Instrument)
        (Logger.LoggerT Warning.Warning
            (Reader.ReaderT Stack.Stack Identity.Identity)))

warn :: String -> ConvertT ()
warn msg = do
    stack <- Reader.ask
    Logger.log (Warning.warning msg stack Nothing)

run_convert :: ConvertT a -> (Maybe a, [Warning.Warning])
run_convert conv = (either (const Nothing) Just val, warn ++ warns)
    where
    run = Identity.runIdentity . flip Reader.runReaderT Stack.empty
        . Logger.run . flip State.runStateT Set.empty . Error.runErrorT
    ((val, _), warns) = run conv
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


{-
verify :: Set.Set String -> Perform.Event -> ConvertT ()
verify allocated event = do
    -- The allocated map uses Score.Instrument since it gets serialized, but
    -- the instruments have already been converted here.  Fortunately, their
    -- names should be the same.
    let event_inst = Instrument.inst_name (Perform.event_instrument event)
    when (event_inst `Set.notMember` allocated) $
        warn ("inst not allocated: " ++ show event_inst)
    return ()

    allocated = (Set.fromList . map Score.inst_name . Map.keys
        . Instrument.config_alloc) config
-}
