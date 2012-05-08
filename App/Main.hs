{-# LANGUAGE ScopedTypeVariables, CPP #-}
-- | Sequencer.
--
-- Dumadak tan wenten alangan.
-- 希望沒有錯誤。
module App.Main where
import qualified Control.Monad.Trans as Trans
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath ((</>))
import qualified Network
import qualified System.Environment
import qualified System.IO as IO

import Util.Control
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import Types
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import qualified Midi.Midi as Midi
import qualified Midi.Interface as Interface

#include "hsconfig.h"
-- This is the actual midi implementation.  This is the only module that should
-- depend on the implementation, so switching backends is relatively easy.
#if defined(CORE_MIDI)
import qualified Midi.CoreMidi as MidiDriver
#else
import qualified Midi.StubMidi as MidiDriver
#endif

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Lang as Lang
import qualified Cmd.LoadMod as LoadMod
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.Responder as Responder
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection

import qualified Derive.Call.All as Call.All
import qualified Derive.Call.Symbols as Call.Symbols
import qualified Derive.Instrument.Symbols as Instrument.Symbols
import qualified Derive.ParseBs as ParseBs
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Symbols as Scale.Symbols

import qualified Instrument.Db as Db

import qualified App.Config as Config
import qualified App.LoadConfig as LoadConfig
import qualified App.StaticConfig as StaticConfig

import qualified Local.Instrument

-- This is only used by the REPL,  but by importing it here I can make
-- sure it, along with REPL-only modules, are compiled and don't have any
-- errors.
import Cmd.Lang.Environ ()

-- tmp, used by debug prints
import qualified Ui.UiTest as UiTest
import qualified Derive.Score as Score
import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch
import qualified Perform.Midi.Instrument as Instrument

import qualified Derive.Derive_profile as Derive_profile


-- * static config

-- Later this moves into its own file.

load_static_config :: IO StaticConfig.StaticConfig
load_static_config = do
    app_dir <- Config.get_app_dir
    instrument_db <- Local.Instrument.load app_dir
    -- Give all the softsynths a default mapping so they're easy to play with.
    let synth_wdevs = mkmap Midi.write_device
            [(dev, iac 1) | dev <- Db.synths instrument_db]
    return $ StaticConfig.StaticConfig {
        StaticConfig.instrument_db = instrument_db
        , StaticConfig.local_lang_dirs = [app_dir </> Config.lang_dir]
        , StaticConfig.global_cmds = []
        , StaticConfig.global_scope = Call.All.scope
        , StaticConfig.setup_cmd = parse_args
        , StaticConfig.rdev_map = rdev_map
        , StaticConfig.wdev_map = wdev_map <> synth_wdevs
        , StaticConfig.read_devices = read_devices
        }

parse_args :: [String] -> Cmd.CmdIO
parse_args argv = case argv of
    [] -> auto_setup_cmd
    ["generate", gen] -> setup_generate gen
    ["mod", fn] -> load_mod fn
    ["-a"] -> do
        Save.cmd_load "save/default"
        State.set_namespace (Id.unsafe_namespace "untitled")
        return Cmd.Done
    [fn]
        | ".git" `List.isSuffixOf` fn -> do
            Save.cmd_load_git fn
            return Cmd.Done
        | otherwise -> do
            Save.cmd_load fn
            return Cmd.Done
    _ -> error $ "bad args: " ++ show argv -- TODO something better

iac n = "IAC Synth " ++ show n
tapco n = "Tapco Port " ++ show n
mkmap mkdev pairs = Map.fromList [(mkdev k, mkdev v) | (k, v) <- pairs]

wdev_map :: Map.Map Midi.WriteDevice Midi.WriteDevice
wdev_map = mkmap Midi.write_device
    [ ("fm8", "Native Instruments FM8 Virtual Input")
    , ("z1", tapco 1)
    , ("vl1", tapco 2)
    , ("morpheus", tapco 2)
    , ("pc2496", tapco 3)
    , ("capybara", tapco 4)
    ]

rdev_map :: Map.Map Midi.ReadDevice Midi.ReadDevice
rdev_map = mkmap Midi.read_device
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morpheus")
    , (tapco 4, "continuum")
    ]

-- | Open these read devices on startup.
read_devices :: Set.Set Midi.ReadDevice
read_devices = Set.fromList $ map Midi.read_device $
    [ "Oxygen USB Oxygen 8 v2"
    , "EDIROL UA-25"
    , "828mk2 MIDI Port"
    ] ++ map tapco [1..4]


-- * main

initialize :: (Network.Socket -> Interface.Interface -> IO ()) -> IO ()
initialize app = do
    log_hdl <- IO.openFile "seq.log" IO.AppendMode
    Log.configure $ const $
        Log.State (Just log_hdl) Log.Debug Log.serialize_msg
    MidiDriver.initialize "seq" $ \interface -> case interface of
        Left err -> error $ "initializing midi: " ++ err
        Right midi_interface -> Network.withSocketsDo $ do
            Config.initialize_lang_port
            socket <- Network.listenOn Config.lang_port
            app socket midi_interface

-- Later, 'load_static_config' is passed as an argument to do_main, and main is
-- defined in Local.hs.
main :: IO ()
main = initialize $ \lang_socket midi_interface -> do
    -- Handy to filter debugging output.
    IO.hSetBuffering IO.stdout IO.LineBuffering
    Log.notice "app starting"
    static_config <- load_static_config
    let loaded_msg = "instrument db loaded, "
            ++ show (Db.size (StaticConfig.instrument_db static_config))
            ++ " instruments loaded"
    Log.notice loaded_msg
    putStrLn loaded_msg

    let _x = _x
    -- satellites are out tonight

    let open_read = StaticConfig.read_devices static_config
    rdevs <- Interface.read_devices midi_interface
    mapM_ (Interface.connect_read_device midi_interface) (Set.toList open_read)
    wdevs <- Interface.write_devices midi_interface
    forM_ wdevs (Interface.connect_write_device midi_interface)
    print_devs open_read rdevs wdevs

    setup_cmd <- StaticConfig.setup_cmd static_config <$>
        System.Environment.getArgs

    -- TODO Sending midi through the whole responder thing is too laggy for
    -- thru.  So give it a shortcut here, but I'll need to give a way to insert
    -- the thru function.  I'll do some responder optimizations first.
    -- thru_chan <- STM.atomically $
    --          STM.dupTChan (Interface.read_channel midi_interface)
    -- Thread.start_logged "midi thru" $
    --     midi_thru remap_rmsg thru_chan write_midi

    loopback_chan <- STM.newTChanIO
    msg_chan <- STM.newTChanIO
    get_msg <- Responder.create_msg_reader
        (remap_read_message (StaticConfig.rdev_map static_config))
        (Interface.read_channel midi_interface) lang_socket msg_chan
        loopback_chan

    startup_initialization

    session <- Lang.make_session
    quit_request <- MVar.newMVar ()
    Thread.start_logged "interpreter" $ do
        Lang.interpreter session
        `Exception.finally` Ui.quit_ui_thread quit_request
        -- ctrl-C is killing this thread now.  The interaction between signals
        -- and OS threads managed by the GHC RTS is probably unpredictable.
        -- I gather the recommended way is to start a thread for signal
        -- handling, I'll do that if this causes more trouble.

    Thread.start_logged "responder" $ do
        let loopback msg = STM.atomically (TChan.writeTChan loopback_chan msg)
        Responder.responder static_config get_msg midi_interface
            setup_cmd session loopback
        `Exception.catch` (\(exc :: Exception.SomeException) ->
            Log.error $ "responder thread died from exception: " ++ show exc)
            -- It would be possible to restart the responder, but chances are
            -- good it would just die again.
        `Exception.finally` Ui.quit_ui_thread quit_request
    Ui.event_loop quit_request msg_chan
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.error $ "ui died from exception: " ++ show exc

    Interface.abort midi_interface
    all_notes_off (Interface.write_message midi_interface) wdevs
    Log.notice "app quitting"

-- | Do one-time startup tasks.
startup_initialization :: IO ()
startup_initialization = do
    LoadConfig.symbols $ Call.Symbols.symbols ++ Scale.Symbols.symbols
        ++ Instrument.Symbols.symbols
    LoadConfig.styles Config.styles
    -- Report keymap and call overlaps.
    mapM_ Log.warn GlobalKeymap.cmd_map_errors
    forM_ shadows $ \(name, shadowed) ->
        Log.warn $ name ++ " calls shadowed: " ++ Pretty.pretty shadowed
    unless (null Scale.All.shadowed) $
        Log.warn $ "scales shadowed: " ++ Pretty.pretty Scale.All.shadowed
    where
    shadows = filter (not . null . snd)
        [ ("note", Call.All.shadowed_notes)
        , ("control", Call.All.shadowed_controls)
        , ("pitch", Call.All.shadowed_pitches)
        , ("val", Call.All.shadowed_vals)
        ]

all_notes_off :: (Midi.WriteMessage -> IO a) -> [Midi.WriteDevice] -> IO ()
all_notes_off write_midi devs = mapM_ write_midi (concat (map msgs devs))
    where
    msgs dev = map (Midi.WriteMessage dev 0) (concat (map off [0..15]))
    off chan = [Midi.ChannelMessage chan Midi.AllNotesOff,
            Midi.ChannelMessage chan Midi.ResetAllControls]

{-
midi_thru remap_rmsg midi_chan write_midi = forever $ do
    rmsg <- fmap remap_rmsg (STM.atomically (STM.readTChan midi_chan))
    let wmsgs = [Midi.WriteMessage dev 0 msg | (dev, msg) <- process_thru rmsg]
    print rmsg
    mapM_ write_midi wmsgs

process_thru :: Midi.ReadMessage -> [(Midi.WriteDevice, Midi.Message)]
process_thru rmsg = [(Midi.WriteDevice "fm8", Midi.rmsg_msg rmsg)]
-}

remap_read_message :: Map.Map Midi.ReadDevice Midi.ReadDevice
    -> Midi.ReadMessage -> Midi.ReadMessage
remap_read_message dev_map rmsg@(Midi.ReadMessage { Midi.rmsg_dev = dev }) =
    rmsg { Midi.rmsg_dev = Map.get dev dev dev_map }

print_devs :: Set.Set Midi.ReadDevice -> [Midi.ReadDevice]
    -> [Midi.WriteDevice] -> IO ()
print_devs opened_rdevs rdevs wdevs = do
    putStrLn "read devs:"
    forM_ rdevs $ \rdev ->
        let prefix = if rdev `Set.member` opened_rdevs then "* " else "  "
        in putStrLn $ prefix ++ show rdev
    putStrLn "write devs:"
    forM_ wdevs $ \wdev ->
        putStrLn $ "* " ++ show wdev


arrival_beats = False

auto_setup_cmd :: Cmd.CmdIO
auto_setup_cmd = setup_normal

setup_generate :: String -> Cmd.CmdIO
setup_generate gen = do
    case gen of
        "simple" -> Derive_profile.make_simple 200
        "nested" -> Derive_profile.make_nested_controls 8 3 64
        "nested-small" -> Derive_profile.make_nested_controls 8 1 64
        "control" -> Derive_profile.make_big_control 15000
        "shared" -> Derive_profile.make_shared_control 2000
        _ -> error gen
    State.set_midi_config $
        make_midi_config "fm8" [("fm8/1", [0..2]), ("fm8/2", [3])]
    Create.view (UiTest.bid "b1")
    Create.map_track_titles set_inst
    return Cmd.Done
    where
    set_inst title
        | title == Derive_profile.inst1 = ">fm8/1"
        | title == Derive_profile.inst2 = ">fm8/2"
        | otherwise = title

load_mod :: String -> Cmd.CmdIO
load_mod fn = do
    blocks <- either Cmd.throw return =<< Trans.liftIO (LoadMod.parse fn)
    let blocks2 = map (LoadMod.map_block (LoadMod.add_default_volume 1 38))
            blocks
    LoadMod.create (Id.unsafe_namespace $ head (Seq.split "." fn))
        (LoadMod.convert_blocks 0.25 blocks2)
    State.set_midi_config $ make_midi_config "ptq" [("ptq/c1", [0..8])]
    return Cmd.Done

setup_normal :: (Cmd.M m) => m Cmd.Status
setup_normal = do
    (bid, vid) <- empty_block
    -- tempo is track 1

    mod <- Create.empty_track bid 2
    State.insert_events mod $ map (control_event . UiTest.make_event)
        [(0, 0, "0"), (1, 0, "i 1"), (2, 0, "i 0"), (2.5, 0, "1"),
            (3, 0, ".5")]
    State.set_track_title mod "modulation"
    State.modify_track_render mod $ \render ->
        render { Track.render_style = Track.Filled }

    note <- Create.empty_track bid 3
    State.insert_events note $ map (note_event . UiTest.make_event)
        [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")]
    State.set_track_title note ">fm8/bass"

    pitch <- Create.empty_track bid 4
    State.insert_events pitch $ map (control_event . UiTest.make_event)
        [(0, 0, "`tr` (5c) 2 3"), (1, 0, "n (5d)"), (2, 0, "5e"),
            (3, 0, "i (5f)")]
    State.set_track_title pitch "*twelve | key = 'c-maj'"
    State.modify_track_render pitch $ \render ->
        render { Track.render_style = Track.Line }
    State.set_track_width bid 3 50

    dyn <- Create.empty_track bid 5
    State.insert_events dyn $ map (control_event . UiTest.make_event)
        [(0, 0, ParseBs.show_hex_val 0.7), (1, 0, ParseBs.show_hex_val 0.4)]
    State.set_track_title dyn "dyn"

    -- tempo 1 -> mod -> note -> pitch
    State.set_skeleton bid $ Skeleton.make [(1, 2), (2, 3), (3, 4), (4, 5)]

    State.set_midi_config (make_midi_config "fm8" [("fm8/bass", [0..2])])
    Selection.set vid (Just (Types.point_selection 0 0))
    return Cmd.Done
    where
    note_event (pos, evt)
        | arrival_beats = (pos+dur, Event.modify_duration negate evt)
        | otherwise = (pos, evt)
        where dur = Event.event_duration evt
    control_event (pos, evt)
        | arrival_beats = (pos + 1, Event.modify_duration negate evt)
        | otherwise = (pos, evt)

setup_big :: (Cmd.M m) => m Cmd.Status
setup_big = do
    (b, view) <- empty_block
    t0 <- Create.empty_track b 2
    State.set_track_title t0 ">fm8/bass"
    t0_p <- Create.empty_track b 3
    State.set_track_title t0_p "p"

    let notes = [0, 3, 2, 5, 3, 6, 4, 7]
        vels = [1, 0.9, 0.8, 0.7, 0.6, 0.4, 0.3, 0.2]
        mknotes notes = map UiTest.make_event
            [(i*0.25, 0.2, to_str (oct*12 + n))
                | (i, (oct, n)) <- zip (Seq.range_ 0 1) notes]
        to_str n = case Twelve.input_to_note Nothing (Pitch.InputKey n) of
            Just (Pitch.Note s) -> s
            Nothing -> error $ "converting " ++ show n
        mkdyn vels = map UiTest.make_event
            [(i*0.25, 0, show vel) | (i, vel) <- zip (Seq.range_ 0 1) vels]

    State.insert_events t0 (take 100 (mknotes (cycle (map ((,) 5) notes))))
    State.insert_events t0_p (take 100 (mkdyn (cycle vels)))

    t1 <- Create.empty_track b 4
    State.set_track_title t1 ">fm8/bass"
    t1_p <- Create.empty_track b 5
    State.set_track_title t1_p "dyn"
    State.insert_events t1
        (take 100 (mknotes (cycle (reverse (map ((,) 6) notes)))))
    State.insert_events t1_p (take 100 (mkdyn (cycle (reverse vels))))

    State.set_midi_config (make_midi_config "fm8" [("fm8/bass", [0..2])])
    State.modify_default $ \d ->
        d { State.default_instrument = Just (Score.Instrument "fm8/bass") }
    Selection.set view (Just (Types.point_selection 0 0))
    return Cmd.Done

empty_block :: (Cmd.M m) => m (BlockId, ViewId)
empty_block = do
    (rid, over_rid) <- Create.ruler "meter44"
        (MakeRuler.ruler [MakeRuler.meter_ruler (1/16) MakeRuler.m44])
        { Ruler.ruler_align_to_bottom = arrival_beats }

    bid <- Create.block rid
    vid <- Create.view bid
    t_tempo <- Create.named_track bid over_rid 1 "tempo"
        (Track.track "tempo" Events.empty)
    State.set_track_width bid 1 40
    State.insert_events t_tempo $ map UiTest.make_event [(0, 0, "1")]
    return (bid, vid)

make_midi_config :: String -> [(String, [Midi.Channel])] -> Instrument.Config
make_midi_config dev config = Instrument.config
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr chan = (Midi.write_device dev, chan)
