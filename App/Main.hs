module App.Main where

import Control.Monad
import qualified Control.Concurrent as Concurrent

import qualified Util.Thread as Thread
import qualified Util.Seq as Seq
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Initialize as Initialize
import qualified Ui.State as State

import qualified Ui.Color as Color
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Midi.Midi as Midi
import qualified Cmd.Responder as Responder

-- tmp
import qualified Ui.Sync as Sync
import qualified Ui.TestSetup as TestSetup
import qualified Ui.Diff as Diff



{-
    Initialize UI
    Initialize MIDI, get midi devs

    Start responder thread.  It has access to: midi devs, midi input chan, ui
    msg input chan.

    Create an empty block with a few tracks.
-}
run_midi app = Midi.initialize (Midi.catch app
    (\ (Midi.Error err) -> Log.error ("midi error: " ++ err)))

main = Initialize.initialize $ \msg_chan -> Midi.initialize $ do
    Log.notice "app starting"
    midi_chan <- Midi.get_read_chan

    let get_msg = Responder.create_msg_reader msg_chan midi_chan
    devs <- Midi.devices
    putStrLn "devices:"
    putStrLn $ "\t" ++ Seq.join "\n\t" (map Midi.device_name devs)

    let input_dev = filter Midi.device_input devs
    when (not (null input_dev)) $ do
        putStrLn $ "open input " ++ Midi.device_name (head input_dev)
        Midi.open_read_device (head (filter Midi.device_input devs))
    let output_dev = filter Midi.device_output devs
    when (not (null output_dev)) $ do
        putStrLn $ "open output " ++ Midi.device_name (head output_dev)
        Midi.open_write_device (head output_dev)

    -- make_test_block
    state <- initial_state
    Responder.responder state get_msg Midi.write_msg

initial_state = do
    res <- State.run State.empty $ do
        ruler <- State.insert_ruler "r1" (TestSetup.mkruler 20 10)
        t1 <- State.insert_track "b1.t1" TestSetup.event_track_1
        b1 <- State.insert_block "b1" (Block.Block "hi b1"
            TestSetup.default_block_config
            (Block.R ruler) [(Block.T t1 ruler, 30)])
        _v1 <- State.insert_view "v1" (Block.View
            b1 TestSetup.default_rect TestSetup.default_view_config [])
        return ()
    let (_, st1, updates) = right res
    sync State.empty st1 updates
    return st1

sync st1 st2 hint_updates = do
    let updates = right $ Diff.diff st1 st2
    print (updates ++ hint_updates)
    result <- Sync.sync st2 (updates ++ hint_updates)
    case result of
        Just err -> putStrLn $ "err: " ++ show err
        Nothing -> putStrLn "synced"

right (Left err) = error $ "error: " ++ show err
right (Right x) = x
