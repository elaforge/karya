module App.Main where

import Control.Monad
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Language.Haskell.Interpreter.GHC as GHC
import qualified System.FilePath as FilePath
import qualified Network
import qualified System.Environment

import qualified Util.Log as Log
import qualified Util.Thread as Thread

import Ui.Types
import qualified Ui.Ui as Ui
import qualified Ui.Block as Block
import qualified Ui.State as State

import qualified Midi.Midi as Midi
import qualified Midi.MidiC as MidiC

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.Responder as Responder
import qualified Cmd.TimeStep as TimeStep

import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig

import qualified Local.Instrument

-- This is only used by the interpreter,  but by importing it here I can make
-- sure it doesn't have any compile errors in advance.
-- TODO but then I have to remove the .o, cuz otherwise ghc insists on failing
-- to load the compiled version.
-- import Cmd.LanguageEnviron ()
import Cmd.LanguageCmds ()

-- tmp
import qualified Ui.TestSetup as TestSetup
import qualified Midi.PortMidi as PortMidi
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument


load_static_config :: FilePath -> IO StaticConfig.StaticConfig
load_static_config app_dir = do
    instrument_db <- Local.Instrument.load $
        FilePath.joinPath [app_dir, "Local", "Instrument"]
    return $ StaticConfig.StaticConfig {
        StaticConfig.config_instrument_db = instrument_db
        , StaticConfig.config_schema_map = Map.empty
        , StaticConfig.config_local_lang_dirs =
            [FilePath.joinPath [app_dir, "Local", "Lang"]]
        , StaticConfig.config_global_cmds = []
        , StaticConfig.config_setup_cmd = setup_cmd
        }

initialize f = do
    Log.initialize "seq.mach.log" "seq.log"
    MidiC.initialize $ \read_chan ->
        Network.withSocketsDo $ do
            Config.initialize_lang_port
            socket <- Network.listenOn Config.lang_port
            f socket read_chan

-- Later, 'load_static_config' is passed as an argument to do_main, and main is
-- defined in Local.hs.
main :: IO ()
main = initialize $ \lang_socket read_chan -> do
    Log.notice "app starting"
    static_config <- load_static_config "."
    Log.notice "instrument db loaded"

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
    msg_chan <- STM.newTChanIO
    get_msg <- Responder.create_msg_reader
        lang_socket msg_chan read_chan player_chan
    let get_ts = fmap MidiC.to_timestamp PortMidi.pt_time
    Log.debug "initialize session"
    session <- GHC.newSession
    quit_request <- MVar.newMVar ()

    args <- System.Environment.getArgs
    let write_midi = write_msg default_stream wdev_streams
        setup_cmd = StaticConfig.config_setup_cmd static_config args

    Thread.start_thread "responder" $ do
        Responder.responder static_config get_msg write_midi get_ts
            player_chan setup_cmd session
        `Exception.catch` responder_handler
            -- It would be possible to restart the responder, but chances are
            -- good it would just die again.
        `Exception.finally` Ui.quit_ui_thread quit_request

    Ui.event_loop quit_request msg_chan

responder_handler exc = do
    Log.error ("responder died from exception: " ++ show exc)
    putStrLn ("responder died from exception: " ++ show exc)

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


setup_cmd :: [String] -> Cmd.CmdIO
setup_cmd _args = do
    (r, over_r) <- Create.ruler
        [MakeRuler.meter_ruler 16 MakeRuler.m44] "meter_44"

    b <- Create.block r
    v <- Create.view b
    Create.named_track b over_r 1 "tempo" "tempo"
    Create.track b 2
    Create.track b 3
    State.set_zoom v (Block.Zoom (TrackPos 0) 0.1)
    return Cmd.Done

old_setup_cmd :: [String] -> Cmd.CmdIO
old_setup_cmd _args = do
    Log.debug "setup block"
    TestSetup.initial_state
    -- Cmd state setup
    State.set_midi_config inst_config
    Cmd.modify_state $ \st -> st
        { Cmd.state_step = TimeStep.UntilMark
            (TimeStep.NamedMarklists ["meter"]) (TimeStep.MatchRank 2)
        }
    return Cmd.Done

inst_config = Instrument.config
        [((Midi.WriteDevice "out", n), Score.Instrument "fm8/bass")
            | n <- [0..2]]
        Nothing
