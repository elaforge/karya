module App.Main where

import Control.Monad
-- import qualified Control.Concurrent as Concurrent
import qualified Data.Map as Map

-- import qualified Util.Thread as Thread
-- import qualified Util.Seq as Seq
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Initialize as Initialize
import qualified Ui.State as State

import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
-- import qualified Ui.Track as Track
-- import qualified Ui.Event as Event

import qualified Midi.Midi as Midi
import qualified Midi.MidiC as MidiC

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Responder as Responder
import qualified Cmd.TimeStep as TimeStep

import qualified App.Config as Config

-- tmp
import qualified Ui.TestSetup as TestSetup
import qualified Midi.PortMidi as PortMidi


{-
    Initialize UI
    Initialize MIDI, get midi devs

    Start responder thread.  It has access to: midi devs, midi input chan, ui
    msg input chan.

    Create an empty block with a few tracks.
-}

main = Initialize.initialize $ \msg_chan -> MidiC.initialize $ \read_chan -> do
    Log.notice "app starting"

    let get_msg = Responder.create_msg_reader msg_chan read_chan

    -- (rdev_map, wdev_map) <- MidiC.devices
    let (rdev_map, wdev_map) = (Map.empty, Map.empty)
            :: (Map.Map Midi.ReadDevice PortMidi.ReadDevice,
                Map.Map Midi.WriteDevice PortMidi.WriteDevice)
    print_devs rdev_map wdev_map

    let rdevs = Map.keys rdev_map
        wdevs = Map.keys wdev_map

    when (not (null rdevs)) $ do
        putStrLn $ "open input " ++ show (head rdevs)
        MidiC.open_read_device read_chan (rdev_map Map.! head rdevs)
    wdev_streams <- case wdevs of
        [] -> return Map.empty
        (wdev:_) -> do
        putStrLn $ "open output " ++ show wdev
        stream <- MidiC.open_write_device (wdev_map Map.! wdev)
        return (Map.fromList [(wdev, stream)])

    Responder.responder get_msg (write_msg wdev_streams) setup_cmd

-- write_msg :: Midi.WriteMessage -> IO ()
write_msg wdev_streams (wdev, ts, msg) =
    MidiC.write_msg (wdev_streams Map.! wdev, MidiC.from_timestamp ts, msg)


print_devs rdev_map wdev_map = do
    putStrLn "read devs:"
    mapM_ print (Map.keys rdev_map)
    putStrLn "write devs:"
    mapM_ print (Map.keys wdev_map)

cues_marklist = Ruler.marklist "cues"
    [ (TrackPos 0, TestSetup.tag "start")
    , (TrackPos 90, TestSetup.tag "head explodes")
    ]

setup_cmd :: Cmd.CmdM
setup_cmd = do
    Log.debug "setup block"
    ruler <- State.create_ruler "r1"
        (TestSetup.ruler [TestSetup.marklist 64 16, cues_marklist])
    overlay <- State.create_ruler "r1.overlay"
        =<< fmap TestSetup.overlay_ruler (State.get_ruler ruler)

    t1 <- State.create_track "b1.t1" TestSetup.event_track_1
    t2 <- State.create_track "b1.t2" TestSetup.event_track_2
    b1 <- State.create_block "b1" (Block.block "hi b1"
        Config.default_block_config
        (Block.R ruler) [(Block.T t1 overlay, 40), (Block.T t2 overlay, 40)])
    v1 <- State.create_view "v1"
        (Block.view b1 TestSetup.default_rect Config.default_view_config)
    State.set_selection v1 0 (Block.point_selection 0 (TrackPos 20))
    _v2 <- State.create_view "v2"
        (Block.view b1 (Block.Rect (500, 30) (200, 200))
            TestSetup.default_view_config)

    -- Cmd state setup
    Cmd.modify_state $ \st -> st
        { Cmd.state_current_step = TimeStep.UntilMark
            (TimeStep.NamedMarklists ["meter"]) (TimeStep.MatchRank 2)
        }
    return Cmd.Done
