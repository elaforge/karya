-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Perform_profile where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map
import qualified System.IO as IO

import qualified Util.Test.Testing as Testing
import qualified Util.Thread as Thread
import qualified Util.TimeVector as TimeVector

import qualified Midi.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.PerformTest as PerformTest
import qualified Perform.Midi.Types as Types
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.InstTypes as InstTypes
import Global


total_events :: Int
total_events = 40 * 1000

signal :: [(Double, Signal.Y)] -> MSignal.Signal
signal = MSignal.from_pairs . map (first RealTime.seconds)

event_count msgs_per_event = floor (fromIntegral total_events / msgs_per_event)

profile_notes = do
    -- simple notes with no controls
    let evts = take (event_count 2) [mkevent n 1 [] pitch | n <- [0..]]
        pitch = MSignal.constant 60
    run_multiple evts $ \arg -> print_msgs $ perform arg

profile_control = do
    -- just perform_control generating control msgs
    let len = 150 * 1000
    let sig = signal (zip [0, 0.25 .. len] (cycle vals))
        vals = map (/10) ([0..10] ++ [10, 9 .. 1])
    let cont = (Controls.mod, sig)
    run_multiple cont $ \arg ->
        print_msgs $ Perform.perform_control Control.empty_map 0 0
            (Just (RealTime.seconds len)) 42 arg

profile_complex = do
    -- notes with pitches and multiple controls, but no multiplexing
    let pitch_at n = signal [(n, fromIntegral (floor n `mod` 64 + 32))]
        mod_sig = signal [(n, n) | n <- [0, 1/16 .. 15/16]]
        mod_at n = (Controls.mod, TimeVector.shift (RealTime.seconds n) mod_sig)
        dynamic_at n = fromIntegral (floor n `mod` 64) / 64 + 1/8
        dyn_at n = (Controls.dynamic, signal [(n, dynamic_at n)])
    let event n = mkevent n 1 [mod_at n, dyn_at n] (pitch_at n)
    -- 16 ccs + 2 notes = 18
    let evts = take (event_count 18) (map event [0,4..])
    run_multiple evts $ \arg -> print_msgs $ perform arg

profile_multiplex = do
    -- notes with non-shareable pitches
    let pitch_sig = signal [(n, n + 64.5) | n <- [0, 1/16 .. 15/16]]
        pitch_at n = TimeVector.shift (RealTime.seconds n) pitch_sig
    let event n = mkevent n 1 [] (pitch_at n)
    let evts = take (event_count 18) (map event [0..])
    run_multiple evts $ \arg -> print_msgs $ perform arg

print_msgs :: (Show msg, DeepSeq.NFData msg, Show log, DeepSeq.NFData log) =>
    ([msg], [log]) -> IO String
print_msgs (msgs, logs) = do
    Testing.force (msgs, logs)
    -- Make failures visible.
    Testing.pprint (take 4 logs)
    Testing.pprint (take 4 msgs)
    return $ show (length msgs) ++ " msgs"

-- * implementation

perform :: [Types.Event] -> ([Midi.WriteMessage], [Text])
perform = split_logs . fst
    . Perform.perform Perform.initial_state configs . map LEvent.Event

split_logs :: [LEvent.LEvent d] -> ([d], [Text])
split_logs = second (map DeriveTest.show_log) . LEvent.partition

run_multiple :: a -> (a -> IO String) -> IO ()
run_multiple arg action = forM_ [1..6] $ \n -> do
    putStr $ show n ++ ": "
    IO.hFlush IO.stdout
    Thread.printTimer (showt n) id (action arg)

mkevent :: Double -> Double -> [(Score.Control, MSignal.Signal)]
    -> MSignal.Signal -> Types.Event
mkevent start dur controls pitch_sig = PerformTest.empty_event
    { Types.event_start = RealTime.seconds start
    , Types.event_duration = RealTime.seconds dur
    , Types.event_patch = patch1
    , Types.event_controls = Map.fromList controls
    , Types.event_pitch = pitch_sig
    }

patch1 :: Types.Patch
patch1 = mkpatch "patch1"

mkpatch :: InstTypes.Name -> Types.Patch
mkpatch name = (PerformTest.mkpatch name) { Types.patch_decay = Just 1 }

configs :: Perform.Configs
configs = Map.fromList
    [ (Score.Instrument "patch1",
        Perform.addrs_config [((dev, n), Nothing) | n <- [0..8]])
    ]
    where dev = Midi.write_device "dev1"
