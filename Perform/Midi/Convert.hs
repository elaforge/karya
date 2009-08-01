{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- ghc confused about Control.Monad
{- | Convert from the Derive events to MIDI performer specific events.

    Since this module depends on both the Derive and Perform.Midi layers, it
    should be called from Derive or Cmd, not Perform.Midi, even though it's
    physically located in Perform.Midi.
-}
module Perform.Midi.Convert where
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Reader as Reader
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Data as Data
import qualified Util.Logger as Logger
import qualified Util.Seq as Seq

import qualified Derive.Schema as Schema
import qualified Derive.Score as Score

import qualified Perform.Warning as Warning
import qualified Perform.Signal as Signal
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Perform as Perform
import qualified Instrument.MidiDb as MidiDb

-- TODO warnings about:
-- - Instrument has a controller that's not in its controller map.
-- - Attributes that match /no/ keyswitches.
-- - No allocation should be warned about in performer?


-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: MidiDb.LookupMidiInstrument -> [Score.Event]
    -> ([Perform.Event], [Warning.Warning])
convert lookup_inst events = (maybe [] id evts, warns)
    where
    (evts, warns) = run_convert $ fmap Maybe.catMaybes $ mapM conv_catch events
    conv_catch event = fmap Just (conv_event event)
        `Error.catchError` (\w -> Logger.record w >> return Nothing)
    conv_event event =
        Reader.local (const (Score.event_stack event))
            (convert_event lookup_inst event)

convert_event :: MidiDb.LookupMidiInstrument -> Score.Event
    -> ConvertT Perform.Event
convert_event lookup_inst event = do
    score_inst <- require "instrument" (Score.event_instrument event)
    midi_inst <- require
        ("midi instrument in instrument db: " ++ show score_inst)
        (lookup_inst (Score.event_attributes event) score_inst)

    (pitch_sig, controllers) <- get_pitch (Score.event_controllers event)
    let perf_cs = Map.insert Controller.c_pitch pitch_sig
            (convert_controllers controllers)
    return $ Perform.Event midi_inst (Score.event_start event)
        (Score.event_duration event) perf_cs (Score.event_stack event)

-- | They're both newtypes so this should boil down to id.
convert_controllers :: Score.ControllerMap -> Perform.ControllerMap
convert_controllers =
    Map.mapKeys (\(Score.Controller c) -> Controller.Controller c)

get_pitch :: Score.ControllerMap
    -> ConvertT (Signal.Signal, Score.ControllerMap)
get_pitch controllers = do
    let pitch_cs = get_pitch_cs controllers
    pitch_sig <- require "pitch" $ case pitch_cs of
        [] -> Nothing
        (_, sig) : _ -> Just sig
    when (length pitch_cs > 1) $
        warn $ "extra pitch tracks ignored: "
            ++ Seq.join ", " (map (show.fst) (drop 1 pitch_cs))
    return (pitch_sig, controllers `Map.difference` Map.fromAscList pitch_cs)

get_pitch_cs :: Score.ControllerMap -> [(Score.Controller, Signal.Signal)]
get_pitch_cs = takeWhile (is_pitch_c . fst) . Map.toAscList
    . snd . Data.split_map (Score.Controller Schema.pitch_track_prefix)

is_pitch_c (Score.Controller c) = Schema.is_pitch_track c

-- * monad

type ConvertT = Error.ErrorT Warning.Warning
    (Logger.LoggerT Warning.Warning
        (Reader.ReaderT Warning.Stack Identity.Identity))

warn :: String -> ConvertT ()
warn msg = do
    stack <- Reader.ask
    Logger.record (Warning.warning msg stack Nothing)

run_convert :: ConvertT a -> (Maybe a, [Warning.Warning])
run_convert conv = (either (const Nothing) Just val, warn ++ warns)
    where
    run = Identity.runIdentity . flip Reader.runReaderT []
        . Logger.run . Error.runErrorT
    (val, warns) = run conv
    warn = either (:[]) (const []) val

require :: String -> Maybe a -> ConvertT a
require msg val = do
    stack <- Reader.ask
    case val of
        Nothing -> Error.throwError $
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
