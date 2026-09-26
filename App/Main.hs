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
import qualified GHC.IO.Encoding as Encoding
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
import qualified Midi.Encode
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Midi.MidiDriver as MidiDriver

import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Repl as Repl
import qualified Cmd.Responder as Responder
import qualified Cmd.SaveGit as SaveGit

import qualified Derive.C.All as C.All
import qualified Derive.Call.Symbols as Call.Symbols
import qualified Derive.Instrument.Symbols as Instrument.Symbols
import qualified Derive.Library as Library
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Symbols as Scale.Symbols

import qualified Instrument.Inst as Inst
import qualified LogView.Tail as Tail

import qualified App.Config as Config
import qualified App.LoadConfig as LoadConfig
import qualified App.StaticConfig as StaticConfig

import qualified Local.Config

import           Global

-- This is only used by the REPL,  but by importing it here I can make
-- sure it, along with REPL-only modules, are compiled and don't have any
-- errors.
import Cmd.Repl.Environ ()

#ifdef ENABLE_IM
import qualified Util.Audio.PortAudio as PortAudio

initializeAudio :: IO a -> IO a
initializeAudio = PortAudio.initialize
#else
initializeAudio :: IO a -> IO a
initializeAudio = id
#endif


initialize :: (Interface.Interface -> Socket.Socket -> IO ()) -> IO ()
initialize app = do
    Encoding.setLocaleEncoding Encoding.utf8
    logFn <- Tail.logFilename
    logHdl <- Log.rotate logFn
    Log.configure $ const $ Log.State
        { stateWriteMsg = Log.writeJson logHdl
        , statePriority = Log.Debug
        }
    MidiDriver.initialize "karya" wantMessage $ \case
        Left err -> errorIO $ "initializing midi: " <> err
        Right midiInterface -> initializeAudio $ Socket.withSocketsDo $ do
            midiInterface <- Interface.trackInterface midiInterface
            Git.initialize $ Repl.withSocket $ app midiInterface
    where
    wantMessage =
        (/= Midi.Encode.encode (Midi.RealtimeMessage Midi.ActiveSense))

main :: IO ()
main = initialize $ \midiInterface replSocket -> do
#ifdef USE_EKG
    System.Remote.Monitoring.forkServer "localhost" 8080
#endif
    -- Handy to filter debugging output.
    IO.hSetBuffering IO.stdout IO.LineBuffering
    Log.notice Tail.startingMsg
    (staticConfig, time) <- Thread.timeActionText
        Local.Config.loadStaticConfig
    let loadedMsg = "loaded "
            <> showt (Inst.size (StaticConfig.instrumentDb staticConfig))
            <> " instruments, in " <> time
    Log.notice loadedMsg
    Text.IO.putStrLn loadedMsg

    let _x = _x -- satellites are out tonight

    let midi = StaticConfig.midi staticConfig
    let openRead = StaticConfig.readDevices midi
    rdevs <- Interface.readDevices midiInterface
    mapM_ (Interface.connectReadDevice midiInterface) (Set.toList openRead)
    wdevs <- Interface.writeDevices midiInterface
    mapM_ (Interface.connectWriteDevice midiInterface . fst) wdevs
    printDevices openRead rdevs wdevs
        (StaticConfig.rdevMap midi) (StaticConfig.wdevMap midi)

    setupCmd <- fmap (<* StaticConfig.getPostSetupCmd staticConfig) $
        either errorIO pure . StaticConfig.setupCmd staticConfig
            =<< System.Environment.getArgs

    -- TODO Sending midi through the whole responder thing is too laggy for
    -- thru.  So give it a shortcut here, but I'll need to give a way to insert
    -- the thru function.  I'll do some responder optimizations first.
    -- thru_chan <- STM.atomically $
    --          STM.dupTChan (Interface.read_channel midi_interface)
    -- Thread.startLogged "midi thru" $
    --     midi_thru remap_rmsg thru_chan write_midi

    loopbackChan <- STM.newTChanIO
    -- Messages from the outside world to the event loop.
    msgChan <- STM.newTChanIO
    getMsg <- Responder.createMsgReader
        (remapReadMessage (StaticConfig.rdevMap midi))
        (Interface.readChannel midiInterface) replSocket msgChan
        loopbackChan

    startupInitialization

    session <- Repl.makeSession
    quitRequest <- MVar.newMVar ()
    uiChan <- MVar.newMVar []
    Thread.startLogged "interpreter" $ do
        Repl.interpreter session
        `Exception.finally` Fltk.quitUiThread quitRequest
        -- ctrl-C is killing this thread now.  The interaction between signals
        -- and OS threads managed by the GHC RTS is probably unpredictable.
        -- I gather the recommended way is to start a thread for signal
        -- handling, I'll do that if this causes more trouble.

    gitUser <- either (\err -> Log.error err >> Processes.exit 1) pure
        =<< SaveGit.getUser
    -- Get screens synchronously, so setup_cmd can see them.
    screens <- BlockC.getScreens
    Thread.startLogged "responder" $ do
        let loopback msg = STM.atomically (TChan.writeTChan loopbackChan msg)
        Responder.responder staticConfig gitUser screens uiChan getMsg
            midiInterface setupCmd session loopback
        `Exception.catch` (\(exc :: Exception.SomeException) ->
            Log.error $ "responder thread died from exception: " <> showt exc)
            -- It would be possible to restart the responder, but chances are
            -- good it would just die again.
        `Exception.finally` Fltk.quitUiThread quitRequest
    Fltk.eventLoop uiChan quitRequest msgChan
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.error $ "ui died from exception: " <> showt exc

    Interface.abort midiInterface
    mapM_ (Interface.writeMessage midiInterface)
        [Interface.AllNotesOff 0, Interface.resetPitch 0]
    Log.notice Tail.quittingMsg

-- | Do one-time startup tasks.
startupInitialization :: IO ()
startupInitialization = do
    LoadConfig.symbols $ concat
        [ Call.Symbols.symbols
        , Scale.Symbols.symbols
        , Instrument.Symbols.symbols
        ]
    LoadConfig.styles Config.styles
    -- Report keymap and call overlaps.
    mapM_ Log.warn GlobalKeymap.allKeymapErrors
    mapM_ (Log.warn . Library.showShadowed) C.All.shadowed
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

remapReadMessage :: Map Midi.ReadDevice Midi.ReadDevice
    -> Midi.ReadMessage -> Midi.ReadMessage
remapReadMessage devMap rmsg@(Midi.ReadMessage { Midi.rmsgDev = dev }) =
    rmsg { Midi.rmsgDev = Map.findWithDefault dev dev devMap }

printDevices :: Set Midi.ReadDevice
    -> [(Midi.ReadDevice, [Midi.ReadDevice])]
    -> [(Midi.WriteDevice, [Midi.WriteDevice])]
    -> Map Midi.ReadDevice Midi.ReadDevice
    -> Map Midi.WriteDevice Midi.WriteDevice
    -> IO ()
printDevices openedRdevs rdevs wdevs rdevMap wdevMap = do
    putStrLn "read devs:"
    forM_ rdevs $ \(rdev, aliases) -> Text.IO.putStrLn $ Text.unwords $
        filter (not . Text.null)
        [ if any (`Set.member` openedRdevs) (rdev : aliases) then "*" else " "
        , pretty rdev
        , if null aliases then "" else pretty aliases
        , maybe "" (("-> "<>) . pretty) $ Map.lookup rdev rdevMap
        ]
    putStrLn "write devs:"
    forM_ wdevs $ \(wdev, aliases) -> Text.IO.putStrLn $ Text.unwords $
        filter (not . Text.null)
        [ "*"
        , pretty wdev
        , if null aliases then "" else pretty aliases
        , maybe "" (("<- "<>) . Text.intercalate ", " . map pretty) $
            Map.lookup wdev wdevToNames
        ]
    where wdevToNames = Maps.multimap $ map Tuple.swap $ Map.toList wdevMap
