-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.PerformTest where
import qualified System.IO as IO

import qualified Util.PPrint as PPrint
import qualified Midi.Midi as Midi
import qualified Cmd.Simple as Simple
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Perform as Perform

import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified Local.Instrument
import qualified App.Config as Config
import Global
import Types



-- | Dump perform events to a Readable format so they can be pasted into
-- a test.  This is analogous to 'Ui.UiTest.to_spec'.
dump_perf_events :: FilePath -> [Perform.Event] -> IO ()
dump_perf_events fname events =
    IO.writeFile fname $ PPrint.pshow (map Simple.dump_exact_perf_event events)

read_perf_events :: [Simple.ExactPerfEvent] -> IO [Perform.Event]
read_perf_events events = do
    db <- Local.Instrument.load =<< Config.get_app_dir
    return $ mapMaybe (Simple.load_exact_perf_event (lookup_patch db)) events

lookup_patch :: Inst.Db code -> InstTypes.Qualified -> Maybe Patch.Patch
lookup_patch db qualified = do
    inst <- Inst.inst_midi =<< Inst.lookup qualified db
    return $ Patch.patch (Score.Instrument (InstTypes.show_qualified qualified))
        inst

empty_event :: Perform.Event
empty_event = Perform.Event
    { event_start = 0
    , event_duration = 0
    , event_patch = patch1
    , event_controls = mempty
    , event_pitch = mempty
    , event_start_velocity = Perform.default_velocity
    , event_end_velocity = Perform.default_velocity
    , event_stack = DeriveTest.fake_stack
    }

patch1, patch2 :: Patch.Patch
patch1 = mkpatch "patch1"
patch2 = mkpatch "patch2"

mkpatch :: Text -> Patch.Patch
mkpatch name = Patch.patch (Score.Instrument ("synth1/" <> name))
    (Instrument.patch (-1, 1) name)


-- * extract

type Extracted a = (Text, RealTime, a)

extract_msg :: (Midi.Message -> Maybe a) -> [Midi.WriteMessage] -> [a]
extract_msg f = map (\(_, _, a) -> a) . extract f

extract_msg_ts :: (Midi.Message -> Maybe a) -> [Midi.WriteMessage]
    -> [(RealTime, a)]
extract_msg_ts f = map (\(_, ts, a) -> (ts, a)) . extract f

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

e_cmsg :: Midi.Message -> Maybe Midi.ChannelMessage
e_cmsg = fmap snd . e_chan_msg

e_cc :: Midi.Control -> Midi.Message -> Maybe Midi.ControlValue
e_cc cc (Midi.ChannelMessage _ (Midi.ControlChange msg_cc val))
    | cc == msg_cc = Just val
e_cc _ _ = Nothing
