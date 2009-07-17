{- | Convert from the Derive events to MIDI performer specific events.

    Since this module depends on both the Derive and Perform.Midi layers, it
    should be called from Derive or Cmd, not Perform.Midi, even though it's
    physically located in Perform.Midi.
-}
module Perform.Midi.Convert where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Util.Logger as Logger

import qualified Derive.Score as Score

import qualified Perform.Warning as Warning
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


-- | Convert Score events to Perform events, emitting warnings that may have
-- happened along the way.
convert :: MidiDb.LookupMidiInstrument -> [Score.Event]
    -> ([Perform.Event], [Warning.Warning])
convert lookup_inst events = (maybe [] id evts, warns)
    where
    (evts, warns) = run_convert $ fmap Maybe.catMaybes $
        mapM conv_event events
    conv_event event = fmap Just (convert_event lookup_inst event)
        `Error.catchError` (\w -> warn w >> return Nothing)

convert_event :: MidiDb.LookupMidiInstrument -> Score.Event
    -> ConvertT Perform.Event
convert_event lookup_inst event = do
    let req = require event
    score_inst <- req "instrument" (Score.event_instrument event)
    midi_inst <- req ("midi instrument in instrument db: " ++ show score_inst)
        (lookup_inst score_inst)
    let controllers = convert_controllers (Score.event_controllers event)
    return $ Perform.Event midi_inst (Score.event_start event)
        (Score.event_duration event) controllers (Score.event_stack event)

-- | They're both newtypes so this should boil down to id.
convert_controllers :: Score.ControllerMap -> Perform.ControllerMap
convert_controllers =
    Map.mapKeys (\(Score.Controller c) -> Controller.Controller c)

-- * monad

type ConvertT = Error.ErrorT Warning.Warning
    (Logger.LoggerT Warning.Warning Identity.Identity)

warn :: Warning.Warning -> ConvertT ()
warn = Trans.lift . Logger.record

run_convert :: ConvertT a -> (Maybe a, [Warning.Warning])
run_convert conv = (either (const Nothing) Just val, warn ++ warns)
    where
    (val, warns) = (Identity.runIdentity . Logger.run . Error.runErrorT) conv
    warn = either (:[]) (const []) val

require :: Score.Event -> String -> Maybe a -> ConvertT a
require event msg val = case val of
    Nothing -> Error.throwError $ Warning.warning
        ("event requires " ++ msg) (Score.event_stack event) Nothing
    Just val -> return val


{-
-- | Warn about non-fatal problems.  The warning is here rather than in perform
-- because this can have a higher-level view, i.e. only warn once about
-- a missing instrument.
--
-- - Instrument of an event is not in the instrument db.
--
-- - TODO Instrument has a controller that's not in its controller map.
--
-- - TODO Attributes that match /no/ keyswitches.
-- UNUSED
verify :: Instrument.Config -> [Perform.Event] -> [String]
verify config events =
    (map show . unique . Maybe.catMaybes . map (verify_event allocated)) events
    where
    allocated = (Set.fromList . map Score.inst_name . Map.keys
        . Instrument.config_alloc) config

unique = Set.toList . Set.fromList

verify_event :: Set.Set String -> Perform.Event -> Maybe String
verify_event allocated event
    | event_inst `Set.notMember` allocated =
        Just ("inst not allocated: " ++ show event_inst)
    | otherwise = Nothing
    where
    -- The allocated map uses Score.Instrument since it gets serialized, but
    -- the instruments have already been converted here.  Fortunately, their
    -- names should be the same.
    event_inst = Instrument.inst_name (Perform.event_instrument event)

-}
