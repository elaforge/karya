{-# LANGUAGE ScopedTypeVariables #-}
module Perform.Midi.PerformTest where
import qualified Data.Map as Map
import qualified System.IO as IO

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Util.ParseBs as ParseBs

import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Signal as Signal

import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb
import qualified Local.Instrument
import qualified App.Config as Config
import Types


-- | Dump perform events to a Readable format so they can be pasted into
-- a test.  This is analogous to 'Ui.UiTest.to_spec'.
dump_perf_events :: FilePath -> [Perform.Event] -> IO ()
dump_perf_events fname events =
    IO.writeFile fname $ PPrint.pshow (map show_perf_event events)

read_perf_events :: [Event] -> IO [Perform.Event]
read_perf_events events = do
    db <- Local.Instrument.load =<< Config.get_app_dir
    return $ mapMaybe
        (make_perf_event (Instrument.Db.db_lookup_midi db)) events

type Event = (String, RealTime, RealTime, [(String, [(RealTime, Signal.Y)])],
    [(RealTime, Signal.Y)], [String])

show_perf_event :: Perform.Event -> Event
show_perf_event (Perform.Event inst start dur controls pitch stack) =
    ( Score.inst_name (Instrument.inst_score inst), start, dur
    , [(k, Signal.unsignal v) | (Control.Control k, v) <- Map.toList controls]
    , Signal.unsignal pitch
    , Stack.to_strings stack
    )

read_perf_event :: MidiDb.LookupMidiInstrument -> String -> Maybe Perform.Event
read_perf_event lookup_inst =
    make_perf_event lookup_inst <=< ParseBs.maybe_read

make_perf_event :: MidiDb.LookupMidiInstrument -> Event -> Maybe Perform.Event
make_perf_event lookup_inst (inst, start, dur, controls, pitch, stack) = do
    (inst, _) <- lookup_inst Score.no_attrs (Score.Instrument inst)
    return $ Perform.Event
        { Perform.event_instrument = inst
        , Perform.event_start = start
        , Perform.event_duration = dur
        , Perform.event_controls = make_controls controls
        , Perform.event_pitch = Signal.signal pitch
        , Perform.event_stack = Stack.from_strings stack
        }

make_controls :: [(String, [(RealTime, Signal.Y)])] -> Perform.ControlMap
make_controls kvs =
    Map.fromList [(Control.Control k, Signal.signal v) | (k, v) <- kvs]
