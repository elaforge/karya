module App.Main where

import Control.Monad
import qualified Control.Concurrent.STM as STM
import qualified Data.Map as Map
import qualified Language.Haskell.Interpreter.GHC as GHC
import qualified Network

import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Initialize as Initialize
import qualified Ui.State as State

import qualified Ui.Block as Block
import qualified Ui.Track as Track

import qualified Midi.Midi as Midi
import qualified Midi.MidiC as MidiC

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Responder as Responder
import qualified Cmd.TimeStep as TimeStep

import qualified App.Config as Config

-- This is only used by the interpreter,  but by importing it here I can make
-- sure it doesn't have any compile errors in advance.
-- TODO but then I have to remove the .o, cuz otherwise ghc insists on failing
-- to load the compiled version.
-- import Cmd.LanguageEnviron ()
import Cmd.LanguageCmds ()

-- tmp
import qualified Ui.TestSetup as TestSetup
import qualified Midi.PortMidi as PortMidi
import qualified Perform.Midi.Instrument as Instrument


{-
    Initialize UI
    Initialize MIDI, get midi devs

    Start responder thread.  It has access to: midi devs, midi input chan, ui
    msg input chan.

    Create an empty block with a few tracks.
-}

initialize f = do
    Log.initialize "seq.mach.log" "seq.log"
    Initialize.initialize $ \msg_chan -> MidiC.initialize $ \read_chan ->
        Network.withSocketsDo $ do
            Config.initialize_lang_port
            socket <- Network.listenOn Config.lang_port
            f socket msg_chan read_chan

main = initialize $ \lang_socket msg_chan read_chan -> do
    Log.notice "app starting"

    (rdev_map, wdev_map) <- MidiC.devices
    print_devs rdev_map wdev_map

    let rdevs = Map.keys rdev_map
        wdevs = Map.keys wdev_map

    -- TODO Don't open IAC ports that I'm opening for writing, otherwise I get
    -- all my msgs bounced back.
    -- when (not (null rdevs)) $ do
    --     putStrLn $ "open input " ++ show (head rdevs)
    --     MidiC.open_read_device read_chan (rdev_map Map.! head rdevs)
    wdev_streams <- case wdevs of
        [] -> return Map.empty
        (wdev:_) -> do
            putStrLn $ "open output " ++ show wdev
            stream <- MidiC.open_write_device (wdev_map Map.! wdev)
            return (Map.fromList [(wdev, stream)])
    let default_stream = head (Map.elems wdev_streams)

    player_chan <- STM.newTChanIO
    get_msg <- Responder.create_msg_reader
        lang_socket msg_chan read_chan player_chan
    let get_ts = fmap MidiC.to_timestamp PortMidi.pt_time
    Log.debug "initialize session"
    session <- GHC.newSession
    Responder.responder get_msg (write_msg default_stream wdev_streams) get_ts
        player_chan setup_cmd session

write_msg :: PortMidi.WriteStream
    -> Map.Map Midi.WriteDevice PortMidi.WriteStream
    -> Midi.WriteMessage
    -> IO ()
write_msg default_stream wdev_streams (Midi.WriteMessage wdev ts msg) = do
    let stream = maybe default_stream id (Map.lookup wdev wdev_streams)
    putStrLn $ "PLAY " ++ show (wdev, ts, msg)
    MidiC.write_msg (stream, MidiC.from_timestamp ts, msg)


print_devs rdev_map wdev_map = do
    putStrLn "read devs:"
    mapM_ print (Map.keys rdev_map)
    putStrLn "write devs:"
    mapM_ print (Map.keys wdev_map)


setup_cmd :: Cmd.CmdId
setup_cmd = do
    Log.debug "setup block"
    TestSetup.initial_state
    -- Cmd state setup
    State.set_midi_config inst_config
    Cmd.modify_state $ \st -> st
        { Cmd.state_step = TimeStep.UntilMark
            (TimeStep.NamedMarklists ["meter"]) (TimeStep.MatchRank 2)
        }
    return Cmd.Done

cont text = Config.event text (TrackPos 0)

inst_config = Instrument.config
        [((Midi.WriteDevice "out", n), inst "fm8" "bass") | n <- [0..2]]
        Nothing

inst synth name = Instrument.instrument synth name Instrument.NoInitialization
    (-12, 12) Nothing
