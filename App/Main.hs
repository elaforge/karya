module App.Main where

import Control.Monad
-- import qualified Control.Concurrent as Concurrent

-- import qualified Util.Thread as Thread
import qualified Util.Seq as Seq
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Initialize as Initialize
import qualified Ui.State as State

-- import qualified Ui.Color as Color
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.Track as Track
import qualified Ui.Event as Event

import qualified Midi.Midi as Midi
import qualified Cmd.Responder as Responder
import qualified Cmd.Cmd as Cmd

-- tmp
import qualified Ui.TestSetup as TestSetup

import qualified Control.Monad.Identity as Identity



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

    Responder.responder get_msg Midi.write_msg setup_cmd

setup_cmd :: Cmd.CmdT Identity.Identity Cmd.Status
setup_cmd = do
    Log.debug "setup block"
    ruler <- State.create_ruler "r1" (TestSetup.mkruler 20 10)
    t1 <- State.create_track "b1.t1" TestSetup.event_track_1
    b1 <- State.create_block "b1" (Block.Block "hi b1"
        TestSetup.default_block_config
        (Block.R ruler) [(Block.T t1 ruler, 30)])
    _v1 <- State.create_view "v1"
        (Block.view b1 TestSetup.default_rect TestSetup.default_view_config)
    return Cmd.Done
