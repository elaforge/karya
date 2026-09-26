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

import qualified Derive.Controls as Controls
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.ScoreT as ScoreT

import qualified Instrument.InstT as InstT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.PerformTest as PerformTest
import qualified Perform.Midi.Types as Types
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import           Global


totalEvents :: Int
totalEvents = 40 * 1000

signal :: [(Double, Signal.Y)] -> MSignal.Signal
signal = MSignal.fromPairs . map (first RealTime.seconds)

eventCount msgsPerEvent = floor (fromIntegral totalEvents / msgsPerEvent)

profileNotes = do
    -- simple notes with no controls
    let evts = take (eventCount 2) [mkevent n 1 [] pitch | n <- [0..]]
        pitch = MSignal.constant 60
    runMultiple evts $ \arg -> printMsgs $ perform arg

profileControl = do
    -- just perform_control generating control msgs
    let len = 150 * 1000
    let sig = signal (zip [0, 0.25 .. len] (cycle vals))
        vals = map (/10) ([0..10] ++ [10, 9 .. 1])
    let cont = (Controls.mod, sig)
    runMultiple cont $ \arg ->
        printMsgs $ Perform.performControl Control.emptyMap 0 0
            (Just (RealTime.seconds len)) 42 arg

profileComplex = do
    -- notes with pitches and multiple controls, but no multiplexing
    let pitchAt n = signal [(n, fromIntegral (floor n `mod` 64 + 32))]
        modSig = signal [(n, n) | n <- [0, 1/16 .. 15/16]]
        modAt n = (Controls.mod, TimeVector.shift (RealTime.seconds n) modSig)
        dynamicAt n = fromIntegral (floor n `mod` 64) / 64 + 1/8
        dynAt n = (Controls.dynamic, signal [(n, dynamicAt n)])
    let event n = mkevent n 1 [modAt n, dynAt n] (pitchAt n)
    -- 16 ccs + 2 notes = 18
    let evts = take (eventCount 18) (map event [0,4..])
    runMultiple evts $ \arg -> printMsgs $ perform arg

profileMultiplex = do
    -- notes with non-shareable pitches
    let pitchSig = signal [(n, n + 64.5) | n <- [0, 1/16 .. 15/16]]
        pitchAt n = TimeVector.shift (RealTime.seconds n) pitchSig
    let event n = mkevent n 1 [] (pitchAt n)
    let evts = take (eventCount 18) (map event [0..])
    runMultiple evts $ \arg -> printMsgs $ perform arg

printMsgs :: (Show msg, DeepSeq.NFData msg, Show log, DeepSeq.NFData log) =>
    ([msg], [log]) -> IO String
printMsgs (msgs, logs) = do
    Testing.force (msgs, logs)
    -- Make failures visible.
    Testing.pprint (take 4 logs)
    Testing.pprint (take 4 msgs)
    pure $ show (length msgs) ++ " msgs"

-- * implementation

perform :: [Types.Event] -> ([Midi.WriteMessage], [Text])
perform = splitLogs . fst
    . Perform.perform Perform.initialState configs . map LEvent.Event

splitLogs :: [LEvent.LEvent d] -> ([d], [Text])
splitLogs = second (map DeriveTest.showLog) . LEvent.partition

runMultiple :: a -> (a -> IO String) -> IO ()
runMultiple arg action = forM_ [1..6] $ \n -> do
    putStr $ show n ++ ": "
    IO.hFlush IO.stdout
    Thread.printTimer (showt n) id (action arg)

mkevent :: Double -> Double -> [(ScoreT.Control, MSignal.Signal)]
    -> MSignal.Signal -> Types.Event
mkevent start dur controls pitchSig = PerformTest.emptyEvent
    { Types.eventStart = RealTime.seconds start
    , Types.eventDuration = RealTime.seconds dur
    , Types.eventPatch = patch1
    , Types.eventControls = Map.fromList controls
    , Types.eventPitch = pitchSig
    }

patch1 :: Types.Patch
patch1 = mkpatch "patch1"

mkpatch :: InstT.Name -> Types.Patch
mkpatch name = (PerformTest.mkpatch name) { Types.patchDecay = Just 1 }

configs :: Perform.Configs
configs = Map.fromList
    [ (ScoreT.Instrument "patch1",
        Perform.addrsConfig [((dev, n), Nothing) | n <- [0..8]])
    ]
    where dev = Midi.writeDevice "dev1"
