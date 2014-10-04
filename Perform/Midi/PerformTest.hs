-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.PerformTest where
import qualified Data.Map as Map
import qualified System.IO as IO

import Util.Control
import qualified Util.PPrint as PPrint
import qualified Util.ParseText as ParseText

import qualified Midi.Midi as Midi
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
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

type Event = (Text, RealTime, RealTime, [(Text, [(RealTime, Signal.Y)])],
    [(RealTime, Signal.Y)], Stack.Stack)

show_perf_event :: Perform.Event -> Event
show_perf_event (Perform.Event inst start dur controls pitch stack) =
    ( Score.inst_name (Instrument.inst_score inst), start, dur
    , [(Score.control_name k, Signal.unsignal v)
        | (k, v) <- Map.toList controls]
    , Signal.unsignal pitch
    , stack
    )

read_perf_event :: MidiDb.LookupMidiInstrument -> String -> Maybe Perform.Event
read_perf_event lookup_inst =
    make_perf_event lookup_inst <=< ParseText.maybe_read

make_perf_event :: MidiDb.LookupMidiInstrument -> Event -> Maybe Perform.Event
make_perf_event lookup_inst (inst, start, dur, controls, pitch, stack) = do
    inst <- lookup_inst (Score.Instrument inst)
    return $ Perform.Event
        { Perform.event_instrument = inst
        , Perform.event_start = start
        , Perform.event_duration = dur
        , Perform.event_controls = make_controls controls
        , Perform.event_pitch = Signal.signal pitch
        , Perform.event_stack = stack
        }

make_controls :: [(Text, [(RealTime, Signal.Y)])] -> Perform.ControlMap
make_controls kvs =
    Map.fromList [(Score.control k, Signal.signal v) | (k, v) <- kvs]


-- * extract

type Extracted a = (Text, RealTime, a)

msg_only :: [Extracted a] -> [a]
msg_only = map $ \(_, _, a) -> a

msg_ts :: [Extracted a] -> [(RealTime, a)]
msg_ts = map $ \(_, ts, a) -> (ts, a)

extract :: (Midi.Message -> Maybe a) -> [Midi.WriteMessage]
    -> [Extracted a]
extract e wmsgs =
    [ (dev, ts, a)
    | (dev, ts, msg) <- extract_midi wmsgs, Just a <- [e msg]
    ]

extract_midi :: [Midi.WriteMessage] -> [Extracted Midi.Message]
extract_midi wmsgs =
    [ (Midi.write_device_text dev, ts, msg)
    | Midi.WriteMessage dev ts msg <- wmsgs
    ]

e_chan_msg :: Midi.Message -> Maybe (Midi.Channel, Midi.ChannelMessage)
e_chan_msg (Midi.ChannelMessage chan msg) = Just (chan, msg)
e_chan_msg _ = Nothing

e_cc :: Midi.Control -> Midi.Message -> Maybe Midi.ControlValue
e_cc cc (Midi.ChannelMessage _ (Midi.ControlChange msg_cc val))
    | cc == msg_cc = Just val
e_cc _ _ = Nothing
