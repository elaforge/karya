module App.Main where

import Control.Monad
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Language.Haskell.Interpreter.GHC as GHC
import System.FilePath ((</>))
import qualified Network
import qualified System.Environment

import qualified Util.Data
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
-- import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Save as Save

import qualified Instrument.Db as Db

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
import qualified Midi.PortMidi as PortMidi
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument


load_static_config :: IO StaticConfig.StaticConfig
load_static_config = do
    app_dir <- Config.get_app_dir
    instrument_db <- Local.Instrument.load app_dir
    return $ StaticConfig.StaticConfig {
        StaticConfig.config_instrument_db = instrument_db
        , StaticConfig.config_schema_map = Map.empty
        , StaticConfig.config_local_lang_dirs = [app_dir </> Config.lang_dir]
        , StaticConfig.config_global_cmds = []
        , StaticConfig.config_setup_cmd = old_setup_cmd
        , StaticConfig.config_read_device_map = read_device_map
        , StaticConfig.config_write_device_map = write_device_map
        }

iac n = "IAC Driver Bus " ++ show n ++ "/CoreMIDI"
tapco n = "Tapco Link MIDI USB Ver 2.2 Port " ++ show n ++ "/CoreMIDI"
mkmap mkdev pairs = Map.fromList [(mkdev k, mkdev v) | (k, v) <- pairs]

write_device_map = mkmap Midi.WriteDevice
    [ ("fm8", iac 1)
    , ("z1", tapco 1)
    , ("vl1", tapco 2)
    , ("morph", tapco 2)
    , ("pc_2496", tapco 3)
    , ("capybara", tapco 4)
    ]

read_device_map = mkmap Midi.ReadDevice
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morph")
    , (tapco 4, "continuum")
    ]

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
    putStrLn "starting"
    static_config <- load_static_config
    let loaded_msg = "instrument db loaded, "
            ++ show (Db.size (StaticConfig.config_instrument_db static_config))
            ++ " instruments loaded"
    Log.notice loaded_msg
    putStrLn loaded_msg

    (rdev_map, wdev_map) <- MidiC.devices
    print_devs rdev_map wdev_map

    wstream_map <- open_write_devices wdev_map (Map.keys wdev_map)
    let default_stream = head (Map.elems wstream_map)
    open_read_devices read_chan rdev_map (Map.keys rdev_map)

    player_chan <- STM.newTChanIO
    msg_chan <- STM.newTChanIO
    get_msg <- Responder.create_msg_reader
        (StaticConfig.config_read_device_map static_config) read_chan
        lang_socket msg_chan player_chan
    let get_ts = fmap MidiC.to_timestamp PortMidi.pt_time
    Log.debug "initialize session"
    putStrLn "initialize session"

    session <- GHC.newSession
    quit_request <- MVar.newMVar ()

    args <- System.Environment.getArgs
    let write_midi = write_msg
            (StaticConfig.config_write_device_map static_config)
            default_stream wstream_map
        setup_cmd = StaticConfig.config_setup_cmd static_config args

    Thread.start_thread "responder" $ do
        Responder.responder static_config get_msg write_midi get_ts
            player_chan setup_cmd session
        `Exception.catch` responder_handler
            -- It would be possible to restart the responder, but chances are
            -- good it would just die again.
        `Exception.finally` Ui.quit_ui_thread quit_request

    Ui.event_loop quit_request msg_chan

open_write_devices :: Map.Map Midi.WriteDevice PortMidi.WriteDevice
    -> [Midi.WriteDevice] -> IO (Map.Map Midi.WriteDevice PortMidi.WriteStream)
open_write_devices wdev_map devs = do
    streams <- mapM (MidiC.open_write_device . (wdev_map Map.!)) devs
    return $ Map.fromList (zip devs streams)

open_read_devices :: MidiC.ReadChan
    -> Map.Map Midi.ReadDevice PortMidi.ReadDevice
    -> [Midi.ReadDevice] -> IO ()
open_read_devices read_chan rdev_map devs = do
    -- Don't open IAC ports that I'm opening for writing, otherwise I get
    -- all my msgs bounced back.
    let ok_devs =
            filter (not . ("IAC " `List.isPrefixOf`) . Midi.un_read_device) devs
    mapM_ (MidiC.open_read_device read_chan . (rdev_map Map.!)) ok_devs

responder_handler exc = do
    Log.error ("responder died from exception: " ++ show exc)
    putStrLn ("responder died from exception: " ++ show exc)

write_msg :: Map.Map Midi.WriteDevice Midi.WriteDevice
    -> PortMidi.WriteStream
    -> Map.Map Midi.WriteDevice PortMidi.WriteStream
    -> Midi.WriteMessage
    -> IO ()
write_msg wdev_map default_stream wstream_map (Midi.WriteMessage wdev ts msg) =
    do
    let real_wdev = Util.Data.get wdev wdev wdev_map
    let stream = maybe default_stream id (Map.lookup real_wdev wstream_map)
    putStrLn $ "PLAY " ++ show (wdev, ts, msg)
    MidiC.write_msg (stream, MidiC.from_timestamp ts, msg)


print_devs rdev_map wdev_map = do
    putStrLn "read devs:"
    mapM_ print (Map.keys rdev_map)
    putStrLn "write devs:"
    mapM_ print (Map.keys wdev_map)


setup_cmd :: [String] -> Cmd.CmdIO
setup_cmd _args = do
    Save.cmd_load "save/default.state"
    State.set_namespace "untitled"
    return Cmd.Done


old_setup_cmd :: [String] -> Cmd.CmdIO
old_setup_cmd _args = do
    (r, over_r) <- Create.ruler
        [MakeRuler.meter_ruler (1/16) MakeRuler.m44] "meter_44"

    b <- Create.block r
    v <- Create.view b
    Create.named_track b over_r 1 "tempo" "tempo"
    Create.track b 2
    Create.track b 3
    State.set_zoom v (Block.Zoom (TrackPos 0) 46)
    return Cmd.Done

inst_config = Instrument.config
        [((Midi.WriteDevice "out", n), Score.Instrument "fm8/bass")
            | n <- [0..2]]
        Nothing
