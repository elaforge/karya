{-# LANGUAGE ScopedTypeVariables, CPP #-}
-- | Sequencer.
--
-- Dumadak tan wenten alangan.
-- 希望沒有錯誤。
module App.Main where

import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.FilePath ((</>))
import qualified Network
import qualified System.Environment
import qualified System.IO as IO
import qualified Text.Printf as Printf

import qualified Util.Map as Map
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import Types
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui

import qualified Midi.Midi as Midi

-- This is the actual midi implementation.  This is the only module that should
-- depend on the implementation, so switching backends is relatively easy.
#if defined(CORE_MIDI)
import qualified Midi.CoreMidi as MidiImp
#else
import qualified Midi.StubMidi as MidiImp
#endif

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.MakeRuler as MakeRuler
import qualified Cmd.Responder as Responder
import qualified Cmd.Save as Save
import qualified Cmd.Selection as Selection
import qualified Cmd.Lang as Lang
import qualified Cmd.LoadMod as LoadMod

import qualified Derive.Call.All as Call.All
import qualified Derive.Call.Symbols as Call.Symbols
import qualified Derive.Scale.Symbols as Scale.Symbols
import qualified Derive.Instrument.Symbols as Instrument.Symbols

import qualified Instrument.Db as Db

import qualified App.Config as Config
import qualified App.LoadConfig as LoadConfig
import qualified App.StaticConfig as StaticConfig

import qualified Local.Instrument

-- This is only used by the interpreter,  but by importing it here I can make
-- sure it doesn't have any compile errors in advance.
-- TODO but then I have to remove the .o, cuz otherwise ghc insists on failing
-- to load the compiled version.
-- import Cmd.Lang.Environ ()

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
    return $ StaticConfig.StaticConfig {
        StaticConfig.config_instrument_db = instrument_db
        , StaticConfig.config_local_lang_dirs = [app_dir </> Config.lang_dir]
        , StaticConfig.config_global_cmds = []
        , StaticConfig.config_global_scope = Call.All.scope
        , StaticConfig.config_setup_cmd = parse_args
        , StaticConfig.config_read_device_map = read_device_map
        , StaticConfig.config_write_device_map = write_device_map
        , StaticConfig.config_read_devices = read_devices
        }

parse_args :: [String] -> Cmd.CmdIO
parse_args argv = case argv of
    [] -> auto_setup_cmd
    ["generate", gen] -> setup_generate gen
    ["mod", fn] -> load_mod fn
    ["-a"] -> do
        Save.cmd_load "save/default"
        State.set_namespace "untitled"
        return Cmd.Done
    [fn] -> do
        Save.cmd_load fn
        return Cmd.Done
    _ -> error $ "bad args: " ++ show argv -- TODO something better

iac n = "IAC Synth " ++ show n
tapco n = "Tapco Port " ++ show n
mkmap mkdev pairs = Map.fromList [(mkdev k, mkdev v) | (k, v) <- pairs]

write_device_map :: Map.Map Midi.WriteDevice Midi.WriteDevice
write_device_map = mkmap Midi.WriteDevice
    [ ("fm8", iac 1)
    , ("ptq", iac 1)

    -- Generic names for loopback ports.  Since it looks like so many
    -- softsynths will just sit on a loopback, I might as well assign that
    -- explicitly.
    , ("loop1", iac 1)
    , ("loop2", iac 2)
    , ("loop3", iac 3)
    , ("loop4", iac 4)
    , ("z1", tapco 1)
    , ("vl1", tapco 2)
    , ("morpheus", tapco 2)
    , ("pc2496", tapco 3)
    , ("capybara", tapco 4)
    ]

read_device_map :: Map.Map Midi.ReadDevice Midi.ReadDevice
read_device_map = mkmap Midi.ReadDevice
    [ (tapco 1, "z1")
    , (tapco 2, "vl1")
    , (tapco 3, "morpheus")
    , (tapco 4, "continuum")
    ]

read_devices :: Set.Set Midi.ReadDevice
read_devices = Set.fromList $ map Midi.ReadDevice $
    [ "Oxygen USB Oxygen 8 v2"
    , "EDIROL UA-25"
    , "828mk2 MIDI Port"
    ] ++ map tapco [1..4]


-- * main

initialize :: (Network.Socket -> MidiImp.ReadChan -> IO ()) -> IO ()
initialize f = do
    log_hdl <- IO.openFile "seq.log" IO.AppendMode
    Log.configure $ const $
        Log.State (Just log_hdl) Log.Debug Log.serialize_msg
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

    let _x = _x
    -- satellites are out tonight

    (rdev_map, wdev_map) <- MidiImp.get_devices
    let open_read = StaticConfig.config_read_devices static_config
    print_devs open_read rdev_map wdev_map
    open_read_devices rdev_map
        (filter (`Set.member` open_read) (Map.keys rdev_map))

    quit_request <- MVar.newMVar ()

    args <- System.Environment.getArgs
    let write_midi = make_write_midi True
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
    -- Thread.start_logged "midi thru" $
    --     midi_thru remap_rmsg thru_chan write_midi

    loopback_chan <- STM.newTChanIO
    msg_chan <- STM.newTChanIO
    get_msg <- Responder.create_msg_reader
        remap_rmsg midi_chan lang_socket msg_chan loopback_chan

    LoadConfig.symbols $ Call.Symbols.symbols ++ Scale.Symbols.symbols
        ++ Instrument.Symbols.symbols
    LoadConfig.styles Config.styles

    session <- Lang.make_session
    Thread.start_logged "interpreter" $ do
        Lang.interpreter session
        `Exception.finally` Ui.quit_ui_thread quit_request
        -- ctrl-C is killing this thread now.  The interaction between signals
        -- and OS threads managed by the GHC RTS is probably unpredictable.
        -- I gather the recommended way is to start a thread for signal
        -- handling, I'll do that if this causes more trouble.

    Thread.start_logged "responder" $ do
        let loopback msg = STM.atomically (TChan.writeTChan loopback_chan msg)
        Responder.responder static_config get_msg write_midi abort_midi
            get_now_ts setup_cmd session loopback
        `Exception.catch` (\(exc :: Exception.SomeException) ->
            Log.error $ "responder thread died from exception: " ++ show exc)
            -- It would be possible to restart the responder, but chances are
            -- good it would just die again.
        `Exception.finally` Ui.quit_ui_thread quit_request
    Ui.event_loop quit_request msg_chan
        `Exception.catch` \(exc :: Exception.SomeException) ->
            Log.error $ "ui died from exception: " ++ show exc

    abort_midi
    let quiet_write = make_write_midi False
            (StaticConfig.config_write_device_map static_config) wdev_map
    all_notes_off quiet_write (Map.keys wdev_map)
    Log.notice "app quitting"

all_notes_off :: (Midi.WriteMessage -> IO ()) -> [Midi.WriteDevice] -> IO ()
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

open_read_devices :: Map.Map Midi.ReadDevice MidiImp.ReadDeviceId
    -> [Midi.ReadDevice] -> IO ()
open_read_devices rdev_map rdevs = forM_ rdevs $ \rdev ->
    MidiImp.connect_read_device rdev (rdev_map Map.! rdev)

make_write_midi :: Bool -> Map.Map Midi.WriteDevice Midi.WriteDevice
    -> MidiImp.WriteMap -> Midi.WriteMessage -> IO ()
make_write_midi noisy wdev_map write_map (Midi.WriteMessage wdev ts msg) = do
    let real_wdev = Map.get wdev wdev wdev_map
    when noisy $
        Printf.printf "PLAY %s->%s %s: %s\n" (Midi.un_write_device wdev)
            (Midi.un_write_device real_wdev) (Pretty.pretty ts) (show msg)
    case Map.lookup real_wdev write_map of
        Nothing -> Log.error $ show real_wdev ++ " not in devs: "
            ++ show (Map.keys write_map)
        Just dev_id -> MidiImp.write_message dev_id ts msg

print_devs :: Set.Set Midi.ReadDevice -> MidiImp.ReadMap -> MidiImp.WriteMap
    -> IO ()
print_devs opened_rdevs rdev_map wdev_map = do
    putStrLn "read devs:"
    forM_ (Map.keys rdev_map) $ \rdev ->
        let prefix = if rdev `Set.member` opened_rdevs then "* " else "  "
        in putStrLn $ prefix ++ show rdev
    putStrLn "write devs:"
    forM_ (Map.keys wdev_map) $ \wdev ->
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
    LoadMod.create (head (Seq.split "." fn))
        (LoadMod.convert_blocks 0.25 blocks2)
    State.set_midi_config $ make_midi_config "loop1" [("ptq/c1", [0..8])]
    return Cmd.Done

setup_normal :: (Cmd.M m) => m Cmd.Status
setup_normal = do
    (bid, vid) <- empty_block
    -- tempo is track 1

    mod <- Create.track bid 2
    State.insert_events mod $ map (control_event . UiTest.make_event)
        [(0, 0, "0"), (1, 0, "i 1"), (2, 0, "i 0"), (2.5, 0, "1"),
            (3, 0, ".5")]
    State.set_track_title mod "modulation"
    State.modify_track_render mod $ \render ->
        render { Track.render_style = Track.Filled }

    note <- Create.track bid 3
    State.insert_events note $ map (note_event . UiTest.make_event)
        [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")]
    State.set_track_title note ">fm8/bass"

    pitch <- Create.track bid 4
    State.insert_events pitch $ map (control_event . UiTest.make_event)
        [(0, 0, "`tr` (5c) 2 3"), (1, 0, "n (5d)"), (2, 0, "5e"),
            (3, 0, "i (5f)")]
    State.set_track_title pitch "*twelve"
    State.modify_track_render pitch $ \render ->
        render { Track.render_style = Track.Line }
    State.set_track_width bid 3 50

    vel <- Create.track bid 5
    State.insert_events vel $ map (control_event . UiTest.make_event)
        [(0, 0, ".7"), (1, 0, ".4")]
    State.set_track_title vel "p"

    -- tempo 1 -> mod -> note -> pitch
    State.set_skeleton bid $ Skeleton.make [(1, 2), (2, 3), (3, 4), (4, 5)]

    State.set_midi_config (make_midi_config "fm8" [("fm8/bass", [0..2])])
    Selection.set vid Config.insert_selnum (Just (Types.point_selection 0 0))
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
    t0 <- Create.track b 2
    State.set_track_title t0 ">fm8/bass"
    t0_p <- Create.track b 3
    State.set_track_title t0_p "p"

    let notes = [0, 3, 2, 5, 3, 6, 4, 7]
        vels = [1, 0.9, 0.8, 0.7, 0.6, 0.4, 0.3, 0.2]
        mknotes notes = map UiTest.make_event
            [(i*0.25, 0.2, to_str (oct*12 + n))
                | (i, (oct, n)) <- zip [0..] notes]
        to_str n = case Twelve.input_to_note (Pitch.InputKey n) of
            Just (Pitch.Note s) -> s
            Nothing -> error $ "converting " ++ show n
        mkvels vels = map UiTest.make_event
            [(i*0.25, 0, show vel) | (i, vel) <- zip [0..] vels]

    State.insert_events t0 (take 100 (mknotes (cycle (map ((,) 5) notes))))
    State.insert_events t0_p (take 100 (mkvels (cycle vels)))

    t1 <- Create.track b 4
    State.set_track_title t1 ">fm8/bass"
    t1_p <- Create.track b 5
    State.set_track_title t1_p "p"
    State.insert_events t1
        (take 100 (mknotes (cycle (reverse (map ((,) 6) notes)))))
    State.insert_events t1_p (take 100 (mkvels (cycle (reverse vels))))

    State.set_midi_config (make_midi_config "fm8" [("fm8/bass", [0..2])])
    State.modify_default $ \d ->
        d { State.default_instrument = Just (Score.Instrument "fm8/bass") }
    Selection.set view Config.insert_selnum (Just (Types.point_selection 0 0))
    return Cmd.Done

empty_block :: (Cmd.M m) => m (BlockId, ViewId)
empty_block = do
    (rid, over_rid) <- Create.ruler "meter_44"
        (MakeRuler.ruler [MakeRuler.meter_ruler (1/16) MakeRuler.m44])
        { Ruler.ruler_align_to_bottom = arrival_beats }

    bid <- Create.block rid
    vid <- Create.view bid
    t_tempo <- Create.named_track bid over_rid 1 "tempo" "tempo"
    State.set_track_width bid 1 40
    State.insert_events t_tempo $ map UiTest.make_event [(0, 0, "1")]
    return (bid, vid)

make_midi_config :: String -> [(String, [Midi.Channel])] -> Instrument.Config
make_midi_config dev config = Instrument.config
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr chan = (Midi.WriteDevice dev, chan)
