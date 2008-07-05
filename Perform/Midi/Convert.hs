{- | Convert from the Derive events to MIDI performer specific events.

Since this module depends on both the Derive and Perform.Midi layers, it should
be called from Derive or Cmd, not Perform.Midi, even though it's physically
located in Perform.Midi.
-}
module Perform.Midi.Convert where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Derive.Score as Score

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Db as Instrument.Db


-- | Events that don't have enough info to be converted to MIDI Events will be
-- returned as Warnings.
convert :: Instrument.Db.LookupMidiInstrument -> [Score.Event]
    -> ([Perform.Event], [Warning.Warning])
convert lookup_inst events = (Maybe.catMaybes midi_events, concat warns)
    where (warns, midi_events) = unzip (map (convert_event lookup_inst) events)

-- | Warn about non-fatal problems.  The warning is here rather than in perform
-- because this can have a higher-level view, i.e. only warn once about
-- a missing instrument.
-- - Instrument of an event is not in the instrument db.
-- - Instrument has a controller that's not in its controller map. TODO
verify :: Instrument.Config -> [Perform.Event] -> [String]
verify config events =
    (map show . unique . Maybe.catMaybes . map (verify_event allocated)) events
    where
    allocated = (Set.fromList . map Score.inst_name . Map.elems
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

convert_event lookup_inst event = case do_convert_event lookup_inst event of
    Left warn -> ([warn], Nothing)
    Right (warns, evt) -> (warns, Just evt)

do_convert_event :: Instrument.Db.LookupMidiInstrument -> Score.Event
    -> Either Warning.Warning ([Warning.Warning], Perform.Event)
do_convert_event lookup_inst event = do
    let req = require event
    inst <- req "instrument" (Score.event_instrument event)
    midi_inst <- req ("midi instrument in instrument db: " ++ show inst)
        (lookup_inst inst)
    pitch <- req "pitch" (Score.event_pitch event)
    let (cwarns, controllers) =
            convert_controllers (Score.event_controllers event)
        controller_warns = map
            (\w -> w { Warning.warn_event = Score.event_stack event })
            cwarns
        start = Timestamp.from_track_pos (Score.event_start event)
        dur = Timestamp.from_track_pos (Score.event_duration event)
    return (controller_warns,
        Perform.Event midi_inst start dur pitch controllers
            (Score.event_stack event))

convert_controllers controllers = ([], Map.fromList mconts)
    where mconts = map convert_controller (Map.assocs controllers)

convert_controller (Score.Controller c, sig)  = (Controller.Controller c, sig)

require event msg Nothing = Left $
    Warning.warning ("event requires " ++ msg) (Score.event_stack event)
        Nothing
require _event _msg (Just x) = Right x
