-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Sequencer.
--
-- Dumadak tan wenten alangan.
module App.Main where
#include "hsconfig.h"
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Tuple as Tuple
import qualified Network.Socket as Socket
import qualified System.Environment
import qualified System.IO as IO
#ifdef USE_EKG
import qualified System.Remote.Monitoring
#endif

import qualified Util.Git as Git
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Processes as Processes
import qualified Util.Thread as Thread

import qualified Ui.BlockC as BlockC
import qualified Ui.Fltk as Fltk
import qualified Midi.Midi as Midi
import qualified Midi.Interface as Interface

-- This is the actual midi implementation.  This is the only module that should
-- depend on the implementation, so switching backends is relatively easy.
#if defined(CORE_MIDI)
import qualified Midi.CoreMidi as MidiDriver
#elif defined(JACK_MIDI)
import qualified Midi.JackMidi as MidiDriver
#else
import qualified Midi.StubMidi as MidiDriver
#endif

import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Repl as Repl
import qualified Cmd.Responder as Responder
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.C.All as C.All
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Symbols as Call.Symbols
import qualified Derive.Instrument.Symbols as Instrument.Symbols
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Symbols as Scale.Symbols

import qualified Instrument.Inst as Inst
import qualified LogView.Tail as Tail

import qualified App.Config as Config
import qualified App.LoadConfig as LoadConfig
import qualified App.StaticConfig as StaticConfig

import qualified Local.Config
import Global

-- This is only used by the REPL,  but by importing it here I can make
-- sure it, along with REPL-only modules, are compiled and don't have any
-- errors.
import Cmd.Repl.Environ ()


initialize :: (Interface.Interface -> Socket.Socket -> IO ()) -> IO ()
initialize app = do
    log_fn <- Tail.log_filename
    log_hdl <- Log.rotate log_fn
    Log.configure $ const $ Log.State
        { state_write_msg = Log.write_json log_hdl
        , state_priority = Log.Debug
        }
    MidiDriver.initialize "seq" want_message $ \interface -> case interface of
        Left err -> errorStack $ "initializing midi: " <> err
        Right midi_interface -> Socket.withSocketsDo $ do
            midi_interface <- Interface.track_interface midi_interface
            Git.initialize $ Repl.with_socket $ app midi_interface
    where
    want_message (Midi.RealtimeMessage Midi.ActiveSense) = False
    want_message _ = True

main :: IO ()
main = initialize $ \midi_interface repl_socket -> do
#ifdef USE_EKG
    System.Remote.Monitoring.forkServer "localhost" 8080
#endif
    -- Handy to filter debugging output.
    IO.hSetBuffering IO.stdout IO.LineBuffering
    Log.notice Tail.starting_msg
    (static_config, time) <- Thread.timeActionText
        Local.Config.load_static_config
    let loaded_msg = "loaded "
            <> showt (Inst.size (StaticConfig.instrument_db static_config))
            <> " instruments, in " <> time
    Log.notice loaded_msg
    Text.IO.putStrLn loaded_msg

    let _x = _x -- satellites are out tonight

    let midi = StaticConfig.midi static_config
    let open_read = StaticConfig.read_devices midi
    rdevs <- Interface.read_devices midi_interface
    mapM_ (Interface.connect_read_device midi_interface) (Set.toList open_read)
    wdevs <- Interface.write_devices midi_interface
    forM_ (map fst wdevs) (Interface.connect_write_device midi_interface)
    print_devs open_read rdevs wdevs
        (StaticConfig.rdev_map midi) (StaticConfig.wdev_map midi)

    setup_cmd <- fmap (<* StaticConfig.post_setup_cmd static_config) $
        either errorIO return . StaticConfig.setup_cmd static_config
            =<< System.Environment.getArgs

    -- TODO Sending midi through the whole responder thing is too laggy for
    -- thru.  So give it a shortcut here, but I'll need to give a way to insert
    -- the thru function.  I'll do some responder optimizations first.
    -- thru_chan <- STM.atomically $
    --          STM.dupTChan (Interface.read_channel midi_interface)
    -- Thread.startLogged "midi thru" $
    --     midi_thru remap_rmsg thru_chan write_midi

    loopback_chan <- STM.newTChanIO
    msg_chan <- STM.newTChanIO
    get_msg <- Responder.create_msg_reader
        (remap_read_message (StaticConfig.rdev_map midi))
        (Interface.read_channel midi_interface) repl_socket msg_chan
        loopback_chan

    startup_initialization

    session <- Repl.make_session
    quit_request <- MVar.newMVar ()
    ui_chan <- MVar.newMVar []
    Thread.startLogged "interpreter" $ do
        Repl.interpreter session
        `Exception.finally` Fltk.quit_ui_thread quit_request
        -- ctrl-C is killing this thread now.  The interaction between signals
        -- and OS threads managed by the GHC RTS is probably unpredictable.
        -- I gather the recommended way is to start a thread for signal
        -- handling, I'll do that if this causes more trouble.

    git_user <- either (\err -> Log.error err >> Processes.exit 1) return
        =<< SaveGit.get_user
    -- Get screens synchronously, so setup_cmd can see them.
    screens <- BlockC.get_screens
    Thread.startLogged "responder" $ do
        let loopback msg = STM.atomically (TChan.writeTChan loopback_chan msg)
        Responder.responder static_config git_user screens ui_chan get_msg
            midi_interface setup_cmd session loopback
        `Exception.catch` (\(exc :: Exception.SomeException) ->
            Log.error $ "responder thread died from exception: " <> showt exc)
            -- It would be possible to restart the responder, but chances are
            -- good it would just die again.
        `Exception.finally` Fltk.quit_ui_thread quit_request
    Fltk.event_loop ui_chan quit_request msg_chan
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.error $ "ui died from exception: " <> showt exc

    Interface.abort midi_interface
    mapM_ (Interface.write_message midi_interface)
        [Interface.AllNotesOff 0, Interface.reset_pitch 0]
    Log.notice Tail.quitting_msg

-- | Do one-time startup tasks.
startup_initialization :: IO ()
startup_initialization = do
    LoadConfig.symbols $ concat
        [ Call.Symbols.symbols
        , Scale.Symbols.symbols
        , Instrument.Symbols.symbols
        ]
    LoadConfig.styles Config.styles
    -- Report keymap and call overlaps.
    mapM_ Log.warn GlobalKeymap.all_keymap_errors
    forM_ C.All.shadowed $
        \((name, Module.Module module_), calls) ->
            Log.warn $ "shadowed " <> name <> " calls in module "
                <> module_ <> ": " <> pretty calls
    unless (null Scale.All.shadowed) $
        Log.warn $ "scales shadowed: " <> pretty Scale.All.shadowed

{-
midi_thru remap_rmsg midi_chan write_midi = forever $ do
    rmsg <- fmap remap_rmsg (STM.atomically (STM.readTChan midi_chan))
    let wmsgs = [Midi.WriteMessage dev 0 msg | (dev, msg) <- process_thru rmsg]
    print rmsg
    mapM_ write_midi wmsgs

process_thru :: Midi.ReadMessage -> [(Midi.WriteDevice, Midi.Message)]
process_thru rmsg = [(Midi.WriteDevice "fm8", Midi.rmsg_msg rmsg)]
-}

remap_read_message :: Map Midi.ReadDevice Midi.ReadDevice
    -> Midi.ReadMessage -> Midi.ReadMessage
remap_read_message dev_map rmsg@(Midi.ReadMessage { Midi.rmsg_dev = dev }) =
    rmsg { Midi.rmsg_dev = Map.findWithDefault dev dev dev_map }

print_devs :: Set Midi.ReadDevice
    -> [(Midi.ReadDevice, [Midi.ReadDevice])]
    -> [(Midi.WriteDevice, [Midi.WriteDevice])]
    -> Map Midi.ReadDevice Midi.ReadDevice
    -> Map Midi.WriteDevice Midi.WriteDevice
    -> IO ()
print_devs opened_rdevs rdevs wdevs rdev_map wdev_map = do
    putStrLn "read devs:"
    forM_ rdevs $ \(rdev, aliases) -> Text.IO.putStrLn $ Text.unwords $
        filter (not . Text.null)
        [ if any (`Set.member` opened_rdevs) (rdev : aliases) then "*" else " "
        , pretty rdev
        , if null aliases then "" else pretty aliases
        , maybe "" (("-> "<>) . pretty) $ Map.lookup rdev rdev_map
        ]
    putStrLn "write devs:"
    forM_ wdevs $ \(wdev, aliases) -> Text.IO.putStrLn $ Text.unwords $
        filter (not . Text.null)
        [ "*"
        , pretty wdev
        , if null aliases then "" else pretty aliases
        , maybe "" (("<- "<>) . Text.intercalate ", " . map pretty) $
            Map.lookup wdev wdev_to_names
        ]
    where wdev_to_names = Maps.multimap $ map Tuple.swap $ Map.toList wdev_map
