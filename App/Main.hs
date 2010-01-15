-- | Sequencer.
--
-- Dumadak tan wenten alangan.
-- 希望沒有錯誤。
module App.Main where

import Control.Monad
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import System.FilePath ((</>))
import qualified Network
import qualified System.Environment
import qualified System.IO as IO
import qualified Text.Printf as Printf

import qualified Util.Map as Map
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import Ui
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import qualified Midi.Midi as Midi
-- This is the actual midi implementation.  This is the only module that should
-- depend on the implementation, so switching backends is relatively easy.
import qualified Midi.CoreMidi as MidiImp

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.Responder as Responder
-- import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Save as Save
import qualified Cmd.Language as Language


import qualified Instrument.Db as Db

import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig

import qualified Local.Instrument
import qualified Local.Instrument.Fm8 as Fm8

-- This is only used by the interpreter,  but by importing it here I can make
-- sure it doesn't have any compile errors in advance.
-- TODO but then I have to remove the .o, cuz otherwise ghc insists on failing
-- to load the compiled version.
-- import Cmd.LanguageEnviron ()
import Cmd.LanguageCmds ()

-- tmp
import qualified Ui.UiTest as UiTest
import qualified Derive.Score as Score
import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch
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
        , StaticConfig.config_setup_cmd = auto_setup_cmd
        , StaticConfig.config_read_device_map = read_device_map
        , StaticConfig.config_write_device_map = write_device_map
        }

iac n = "IAC Out " ++ show n
tapco n = "Tapco " ++ show n
mkmap mkdev pairs = Map.fromList [(mkdev k, mkdev v) | (k, v) <- pairs]

write_device_map = mkmap Midi.WriteDevice
    [ ("fm8", iac 1)
    , ("z1", tapco 1)
    , ("vl1", tapco 2)
    , ("morpheus", tapco 2)
    , ("pc_2496", tapco 3)
    , ("capybara", tapco 4)
    ]

read_device_map = mkmap Midi.ReadDevice
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morpheus")
    , (tapco 4, "continuum")
    ]

initialize :: (Network.Socket -> MidiImp.ReadChan -> IO ()) -> IO ()
initialize f = do
    Log.initialize (Just "seq.log") Log.Timer
    MidiImp.initialize $ \midi_chan ->
        Network.withSocketsDo $ do
            Config.initialize_lang_port
            socket <- Network.listenOn Config.lang_port
            f socket midi_chan

-- Later, 'load_static_config' is passed as an argument to do_main, and main is
-- defined in Local.hs.
main :: IO ()
main = initialize $ \lang_socket midi_chan -> do
    -- Handy to filter debugging output.
    IO.hSetBuffering IO.stdout IO.LineBuffering
    Log.notice "app starting"
    static_config <- load_static_config
    let loaded_msg = "instrument db loaded, "
            ++ show (Db.size (StaticConfig.config_instrument_db static_config))
            ++ " instruments loaded"
    Log.notice loaded_msg
    putStrLn loaded_msg

    (rdev_map, wdev_map) <- MidiImp.get_devices
    print_devs rdev_map wdev_map
    -- Don't open out IAC ports for read, otherwise any msgs written to them
    -- will bounce back.
    let is_rdev = not . ("IAC Out" `List.isPrefixOf`) . Midi.un_read_device
    open_read_devices rdev_map (filter is_rdev (Map.keys rdev_map))
    putStrLn $ "read devs opened " ++ show (filter is_rdev (Map.keys rdev_map))

    quit_request <- MVar.newMVar ()

    args <- System.Environment.getArgs
    let write_midi = make_write_midi
            (StaticConfig.config_write_device_map static_config) wdev_map
        setup_cmd = StaticConfig.config_setup_cmd static_config args
        abort_midi = MidiImp.abort
        remap_rmsg = remap_read_message
            (StaticConfig.config_read_device_map static_config)
        get_now_ts = MidiImp.now

    -- TODO Sending midi through the whole responder thing is too laggy for
    -- thru.  So give it a shortcut here, but I'll need to give a way to insert
    -- the thru function.  I'll do some responder optimizations first.
    -- thru_chan <- STM.atomically (STM.dupTChan midi_chan)
    -- Thread.start_thread "midi thru" $
    --     midi_thru remap_rmsg thru_chan write_midi

    player_chan <- STM.newTChanIO
    msg_chan <- STM.newTChanIO
    get_msg <- Responder.create_msg_reader
        remap_rmsg midi_chan lang_socket msg_chan player_chan

    interpreter_chan <- Chan.newChan
    Thread.start_thread "interpreter" $ do
        Language.interpreter interpreter_chan
        `Exception.finally` Ui.quit_ui_thread quit_request
        -- ctrl-C is killing this thread now.  The interaction between signals
        -- and OS threads managed by the GHC RTS is probably unpredictable.
        -- I gather the recommended way is to start a thread for signal
        -- handling, I'll do that if this causes more trouble.

    Thread.start_thread "responder" $ do
        Responder.responder static_config get_msg write_midi abort_midi
            get_now_ts player_chan setup_cmd interpreter_chan
        `Exception.catch` responder_handler
            -- It would be possible to restart the responder, but chances are
            -- good it would just die again.
        `Exception.finally` Ui.quit_ui_thread quit_request

    Ui.event_loop quit_request msg_chan
    Log.notice "app quitting"

{-
midi_thru remap_rmsg midi_chan write_midi = forever $ do
    rmsg <- fmap remap_rmsg (STM.atomically (STM.readTChan midi_chan))
    let wmsgs = [Midi.WriteMessage dev Timestamp.immediately msg
            | (dev, msg) <- process_thru rmsg]
    print rmsg
    mapM_ write_midi wmsgs

process_thru :: Midi.ReadMessage -> [(Midi.WriteDevice, Midi.Message)]
process_thru rmsg = [(Midi.WriteDevice "fm8", Midi.rmsg_msg rmsg)]
-}

remap_read_message dev_map rmsg@(Midi.ReadMessage { Midi.rmsg_dev = dev }) =
    rmsg { Midi.rmsg_dev = Map.get dev dev dev_map }

open_read_devices :: Map.Map Midi.ReadDevice MidiImp.ReadDeviceId
    -> [Midi.ReadDevice] -> IO ()
open_read_devices rdev_map rdevs = forM_ rdevs $ \rdev ->
    MidiImp.connect_read_device rdev (rdev_map Map.! rdev)

responder_handler :: Exception.SomeException -> IO ()
responder_handler exc = do
    Log.error ("responder died from exception: " ++ show exc)
    putStrLn ("responder died from exception: " ++ show exc)

make_write_midi :: Map.Map Midi.WriteDevice Midi.WriteDevice
    -> MidiImp.WriteMap -> Midi.WriteMessage -> IO ()
make_write_midi wdev_map write_map (Midi.WriteMessage wdev ts msg) = do
    let real_wdev = Map.get wdev wdev wdev_map
    Printf.printf "PLAY %s->%s: %s\n" (Midi.un_write_device wdev)
        (Midi.un_write_device real_wdev) (show msg)
    case Map.lookup real_wdev write_map of
        Nothing -> Log.error $ show real_wdev ++ " not in devs: "
            ++ show (Map.keys write_map)
        Just dev_id -> do
            MidiImp.write_message dev_id ts msg

print_devs :: MidiImp.ReadMap -> MidiImp.WriteMap -> IO ()
print_devs rdev_map wdev_map = do
    putStrLn "read devs:"
    mapM_ print (Map.keys rdev_map)
    putStrLn "write devs:"
    mapM_ print (Map.keys wdev_map)


setup_cmd :: [String] -> Cmd.CmdIO
setup_cmd _args = do
    Save.cmd_load "save/default"
    State.set_project "untitled"
    return Cmd.Done

arrival_beats = False

auto_setup_cmd :: [String] -> Cmd.CmdIO
auto_setup_cmd _args = do
    (bid, vid) <- empty_block
    t0 <- Create.track bid 2
    State.insert_events t0 $ map (note_event . UiTest.mkevent)
        [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")]
    State.set_track_title t0 ">fm8/bass"
    t1 <- Create.track bid 3
    State.insert_events t1 $ map (control_event . UiTest.mkevent)
        [(0, 0, "5c"), (1, 0, "5d"), (2, 0, "5e"), (3, 0, "5f")]
    State.set_track_title t1 "*twelve"
    State.set_track_width vid 3 50
    -- tempo 1 -> *twelve 3 -> >fm8/bass 2
    State.set_skeleton bid $ Skeleton.make [(1, 3), (3, 2)]

    State.set_midi_config inst_config
    State.set_selection vid Config.insert_selnum
        (Types.point_selection 0 (TrackPos 0))
    return Cmd.Done
    where
    note_event (pos, evt)
        | arrival_beats = (pos+dur, Event.modify_duration negate evt)
        | otherwise = (pos, evt)
        where dur = Event.event_duration evt
    control_event (pos, evt)
        | arrival_beats = (pos + 1, Event.modify_duration negate evt)
        | otherwise = (pos, evt)

setup_big :: [String] -> Cmd.CmdIO
setup_big _ = do
    (b, view) <- empty_block
    t0 <- Create.track b 2
    State.set_track_title t0 ">fm8/bass"
    t0_vel <- Create.track b 3
    State.set_track_title t0_vel "velocity"

    let notes = [0, 3, 2, 5, 3, 6, 4, 7]
        vels = [1, 0.9, 0.8, 0.7, 0.6, 0.4, 0.3, 0.2]
        mknotes notes = map UiTest.mkevent
            [(i*0.25, 0.2, to_str (oct*12 + n))
                | (i, (oct, n)) <- zip [0..] notes]
        to_str n = case Twelve.input_to_note (Pitch.InputKey n) of
            Just (Pitch.Note s) -> s
            Nothing -> error $ "converting " ++ show n
        mkvels vels = map UiTest.mkevent
            [(i*0.25, 0, show vel) | (i, vel) <- zip [0..] vels]

    State.insert_events t0 (take 100 (mknotes (cycle (map ((,) 5) notes))))
    State.insert_events t0_vel (take 100 (mkvels (cycle vels)))

    t1 <- Create.track b 4
    State.set_track_title t1 ">fm8/bass"
    t1_vel <- Create.track b 5
    State.set_track_title t1_vel "velocity"
    State.insert_events t1
        (take 100 (mknotes (cycle (reverse (map ((,) 6) notes)))))
    State.insert_events t1_vel (take 100 (mkvels (cycle (reverse vels))))

    State.set_midi_config inst_config
    State.set_selection view Config.insert_selnum
        (Types.point_selection 0 (TrackPos 0))
    return Cmd.Done

empty_block = do
    (rid, over_rid) <- Create.ruler "meter_44"
        (MakeRuler.ruler [MakeRuler.meter_ruler (1/16) MakeRuler.m44])
        { Ruler.ruler_align_to_bottom = arrival_beats }

    bid <- Create.block rid
    vid <- Create.view bid
    t_tempo <- Create.named_track bid over_rid 1 "tempo" "tempo"
    State.set_track_width vid 1 40
    State.insert_events t_tempo $ map UiTest.mkevent [(0, 0, "1")]
    return (bid, vid)

inst_config =
    Instrument.config [(Score.Instrument "fm8/bass", addrs)] Nothing
    where addrs = [(Instrument.synth_device Fm8.fm8, n) | n <- [0..2]]
