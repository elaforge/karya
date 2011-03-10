module Perform.Midi.Perform_profile where
import Control.Monad
import qualified Data.Map as Map
import qualified System.IO as IO

import Util.Control
import Util.Test

import qualified Midi.Midi as Midi

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Instrument as Instrument

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


total_events :: Int
total_events = 40 * 1000

signal = Signal.signal . map (first RealTime.seconds)

event_count msgs_per_event = floor (fromIntegral total_events / msgs_per_event)

profile_notes = do
    -- simple notes with no controls
    let evts = take (event_count 2) [mkevent n 1 [] Signal.empty | n <- [0..]]
    run_multiple evts $ \arg -> do
        let (msgs, logs) = perform arg
        force logs
        force msgs
        return $ show (length msgs) ++ " msgs"

profile_control = do
    -- just perform_control generating control msgs
    let len = 150 * 1000
    let sig = signal (zip [0, 0.25 .. len] (cycle vals))
        vals = map (/10) ([0..10] ++ [10, 9 .. 1])
    let cont = (Control.Control Control.c_mod, sig)
    run_multiple cont $ \arg -> do
        let (msgs, warns) = Perform.perform_control Control.empty_map
                Timestamp.zero Timestamp.zero arg
        force warns
        force msgs
        return $ show (length msgs) ++ " msgs"

profile_complex = do
    -- notes with pitches and multiple controls, but no multiplexing
    let pitch_at n = signal [(n, fromIntegral (floor n `mod` 64 + 32))]
        mod_sig = signal [(n, n) | n <- [0, 1/16 .. 15/16]]
        mod_at n = (Control.Control Control.c_mod,
            Signal.shift (RealTime.seconds n) mod_sig)
        velocity_at n = fromIntegral (floor n `mod` 64) / 64 + 1/8
        vel_at n = (Control.c_velocity, signal [(n, velocity_at n)])
    let event n = mkevent n 1 [mod_at n, vel_at n] (pitch_at n)
    -- 16 ccs + 2 notes = 18
    let evts = take (event_count 18) (map event [0,4..])
    run_multiple evts $ \arg -> do
        let (msgs, logs) = perform arg
        force logs
        force msgs
        return $ show (length msgs) ++ " msgs"

profile_multiplex = do
    -- notes with non-shareable pitches
    let pitch_sig = signal [(n, n + 64.5) | n <- [0, 1/16 .. 15/16]]
        pitch_at n = Signal.shift (RealTime.seconds n) pitch_sig
    let event n = mkevent n 1 [] (pitch_at n)
    let evts = take (event_count 18) (map event [0..])
    run_multiple evts $ \arg -> do
        let (msgs, logs) = perform arg
        force logs
        force msgs
        return $ show (length msgs) ++ " msgs"


perform :: [Perform.Event] -> ([Midi.WriteMessage], [String])
perform = split_logs . fst
    . Perform.perform Perform.initial_state midi_config . map LEvent.Event

split_logs = second (map (DeriveTest.show_log)) . LEvent.partition

run_multiple arg action = forM_ [1..6] $ \n -> do
    putStr $ (show n) ++ ": "
    IO.hFlush IO.stdout
    print_timer (show n) (action arg)


mkevent :: Double -> Double -> [(Control.Control, Signal.Control)]
    -> Signal.NoteNumber -> Perform.Event
mkevent start dur controls pitch_sig =
    Perform.Event inst1 (Timestamp.seconds start)
        (Timestamp.seconds dur) (Map.fromList controls) pitch_sig Stack.empty

inst1 = mkinst "inst1"
mkinst name = (Instrument.instrument name [] (-2, 2))
    { Instrument.inst_score = Score.Instrument name
    , Instrument.inst_maybe_decay = Just 1
    }

midi_config =
    Instrument.config [(Score.Instrument "inst1", [(dev, n) | n <- [0..8]])]
    where dev = Midi.WriteDevice "dev1"
