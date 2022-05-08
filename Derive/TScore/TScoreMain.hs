-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Standalone driver for tscore.
module Derive.TScore.TScoreMain where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Tuple as Tuple
import qualified Data.Vector as Vector

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified Util.Audio.PortAudio as PortAudio
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified App.Config as App.Config
import qualified App.Path as Path
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Ky as Ky
import qualified Cmd.Performance as Performance
import qualified Cmd.SaveGitT as SaveGitT
import qualified Cmd.Simple as Simple

import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.LEvent as LEvent
import qualified Derive.Parse.Instruments as Instruments
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.TScore.T as T
import qualified Derive.TScore.TScore as TScore

import qualified Instrument.Inst as Inst
import qualified Local.Config
import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Midi.MidiDriver as MidiDriver

import qualified Perform.Im.Convert as Im.Convert
import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Sc.Note as Sc.Note
import qualified Perform.Sc.Patch as Sc.Patch
import qualified Perform.Sc.Play as Sc.Play
import qualified Perform.Transport as Transport

import qualified Synth.StreamAudio as StreamAudio
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


-- TODO this compiles Cmd.GlobalKeymap, why?
-- SyncKeycaps -> User.Elaforge.Config -> Local.Config -> DeriveSaved
--
-- So I must either split config into interactive and non-interactive, or have
-- some hack to open keycaps without directly calling SyncKeycaps.

-- * main

main :: IO ()
main = do
    Log.configure $ \state -> state { Log.state_priority = Log.Notice }
    (flags, args, errors) <- GetOpt.getOpt GetOpt.Permute options <$>
        Environment.getArgs
    unless (null errors) $ usage errors
    if  | Check `elem` flags -> mapM_ check_score args
        | Dump `elem` flags -> mapM_ dump_score args
        | List `elem` flags -> list_devices
        | otherwise -> case args of
            [fname] -> play_score (Seq.last [d | Device d <- flags]) fname
            _ -> usage []
    where
    usage errors = die $ Text.stripEnd $ Text.unlines $ map txt errors ++
        [ "usage: tscore [ flags ] input.tscore"
        , txt $ dropWhile (=='\n') $ GetOpt.usageInfo "" options
        ]

data Flag = Check | Dump | List | Device String
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["check"] (GetOpt.NoArg Check) "check score only"
    , GetOpt.Option [] ["device"] (GetOpt.ReqArg Device "dev")
        "use named device"
    , GetOpt.Option [] ["dump"] (GetOpt.NoArg Dump) "dump score"
    , GetOpt.Option [] ["list"] (GetOpt.NoArg List) "list output devices"
    ]

die :: Text -> IO a
die msg = do
    Text.IO.hPutStrLn IO.stderr msg
    Exit.exitFailure

get_device :: String -> IO StreamAudio.Device
get_device name = do
    (devs, _) <- StreamAudio.getDevices
    maybe (errorIO $ "unknown device: " <> showt name) (return . snd) $
        List.find ((==name) . fst) devs

initialize_midi :: (Interface.Interface -> IO a) -> IO a
initialize_midi app = MidiDriver.initialize "tscore" (const False) $ \case
    Left err -> die $ "error initializing midi: " <> err
    Right midi_interface -> app =<< Interface.track_interface midi_interface

check_score :: FilePath -> IO ()
check_score fname = do
    source <- Text.IO.readFile fname
    case TScore.parse_score source of
        Left err -> Text.IO.putStrLn $ txt fname <> ": " <> err
        Right (ui_state, _allocs) ->
            Text.IO.putStr $ Transform.show_stats ui_state

-- TODO show duration?  I have to derive for that though.
-- Maybe check should derive!

score_stats :: T.Score -> Text
score_stats (T.Score toplevels) = Text.unwords
    [ showt (length blocks), "blocks"
    , showt (length tracks), "tracks"
    , showt (length notes) <> "/" <> showt (length rests), "notes/rests"
    ]
    where
    blocks = [b | (_, T.BlockDefinition b) <- toplevels]
    tracks =
        [ t | b <- blocks, T.WrappedTracks _ wraps <- [T.block_tracks b]
        , T.Tracks ts <- wraps, t <- ts
        ]
    notes = [n | t <- tracks, T.TNote _ n <- T.track_tokens t]
    rests = [n | t <- tracks, T.TRest _ n <- T.track_tokens t]

list_devices :: IO ()
list_devices = PortAudio.initialize $ initialize_midi $ \midi_interface -> do
    putStrLn "Audio devices:"
    (devs, default_dev) <- StreamAudio.getDevices
    forM_ (map fst devs) $ \dev ->
        putStrLn $ (if dev == default_dev then "* " else "  ") <> show dev
    putStrLn "Midi devices:"
    wdevs <- Interface.write_devices midi_interface
    static_config <- Local.Config.load_static_config
    print_midi_devices wdevs
        (StaticConfig.wdev_map (StaticConfig.midi static_config))

dump_score :: FilePath -> IO ()
dump_score fname = do
    source <- Text.IO.readFile fname
    cmd_config <- DeriveSaved.load_cmd_config
    (ui_state, cmd_state) <- either die return =<< load_score cmd_config source
    dump <- either (die . pretty) return $
        Ui.eval ui_state Simple.dump_state
    Pretty.pprint dump
    putStrLn "\n\tscore events:"
    block_id <- maybe (die "no root block") return $
        Ui.config#UiConfig.root #$ ui_state
    let (events, logs) = derive ui_state cmd_state block_id
    mapM_ Log.write logs
    mapM_ (Text.IO.putStrLn . Score.short_event) events

    events <- dump_im ui_state cmd_state block_id events
    let ((midi_msgs, sc_msgs), logs) =
            DeriveSaved.perform cmd_state ui_state events
    mapM_ Log.write logs
    unless (null midi_msgs) $ do
        putStrLn "\n\tmidi msgs:"
        mapM_ Pretty.pprint midi_msgs
    unless (null sc_msgs) $ do
        putStrLn "\n\tsc msgs:"
        mapM_ Pretty.pprint sc_msgs

dump_im :: Ui.State -> Cmd.State -> BlockId -> Vector.Vector Score.Event
    -> IO (Vector.Vector Score.Event)
dump_im ui_state cmd_state block_id events = do
    unless (null im_notes && null logs) $ do
        putStrLn "\n\tim events:"
        mapM_ Log.write logs
        mapM_ Pretty.pprint im_notes
    return rest_events
    where
    (im_events, rest_events) = Vector.partition is_im_event events
    (im_notes, logs) = LEvent.partition $
        Im.Convert.convert block_id lookup_inst $ Vector.toList im_events
    is_im_event =
        maybe False (is_im . Cmd.inst_instrument) . lookup_inst
            . Score.event_instrument
    is_im (Inst.Inst (Inst.Im {}) _) = True
    is_im _ = False
    lookup_inst = either (const Nothing) Just
        . Cmd.state_lookup_instrument ui_state cmd_state

play_score :: Maybe String -> FilePath -> IO ()
play_score mb_device fname = PortAudio.initialize $ initialize_midi $
        \midi_interface -> do
    source <- Text.IO.readFile fname
    cmd_config <- load_cmd_config midi_interface
    (ui_state, cmd_state) <- either die return =<< load_score cmd_config source
    initialize_instruments ui_state cmd_state

    block_id <- maybe (die "no root block") return $
        Ui.config#UiConfig.root #$ ui_state
    let (im_events, logs) = derive ui_state cmd_state block_id
    mapM_ Log.write logs

    let start = 0 -- TODO from cmdline

    let score_path = fname
    (procs, events) <- perform_im score_path cmd_state ui_state im_events
        block_id
    play_ctl <- Transport.play_control
    players <- Transport.active_players
    unless (null procs) $ do
        mb_device <- traverse get_device mb_device
        play_im mb_device score_path players play_ctl block_id start im_events
            (get_im_instruments ui_state) procs

    let ((midi_msgs, sc_msgs), logs) =
            DeriveSaved.perform cmd_state ui_state events
    mapM_ Log.write logs
    unless (null sc_msgs) $ Sc.Play.play
        (Sc.Play.State { _play_control = play_ctl, _players = players })
        (Sc.Note.PlayNotes { shift = 0, stretch = 1, notes = sc_msgs })
        Nothing

    -- TODO since I have to wait for im, I may have to bump MIDI forward.
    -- Of course they won't be very in sync anyway...
    unless (null midi_msgs) $
        play_midi play_ctl players midi_interface cmd_state ui_state midi_msgs

    Thread.startLogged "kbd" $ do
        putStrLn "press return to stop player"
        _ <- IO.getLine
        Transport.stop_player play_ctl
    putStrLn "waiting for players to complete..."
    Transport.wait_player_stopped players
    Thread.delay 0.1 -- let threads print final logs before exiting
    putStrLn "done"

initialize_instruments :: Ui.State -> Cmd.State -> IO ()
initialize_instruments ui_state cmd_state = do
    (result, logs) <- Cmd.eval ui_state cmd_state sc_initialize
    case result of
        Left err -> Log.warn $ txt err
        Right () -> return ()
    mapM_ Log.write logs

-- | Ask scsynth to load the patches.  TODO: if this gets slow, is there some
-- way to detect if the server is up to date and skip this?
sc_initialize :: Cmd.CmdT IO ()
sc_initialize = do
    insts <- Ui.get_config $ Map.keys . UiConfig.unallocations
        . UiConfig.config_allocations
    insts <- mapM Cmd.get_instrument insts
    liftIO $ Sc.Play.add_default_group
    liftIO $ sc_initialize_patches $ mapMaybe Cmd.sc_patch insts

sc_initialize_patches :: [Sc.Patch.Patch] -> IO ()
sc_initialize_patches patches = Sc.Play.version >>= \case
    Left err -> errorIO $ "can't initialize sc patches: " <> err
    Right msg -> do
        Text.IO.putStrLn $ "found scsynth: " <> msg
        Text.IO.putStrLn $ "loading patches: "
            <> Text.unwords (map (Texts.toText . Sc.Patch.name) patches)
        mapM_ Sc.Play.initialize_patch patches
        ((), dur) <- Thread.timeActionText $ Sc.Play.sync
        Text.IO.putStrLn $ "sync took: " <> dur

-- * midi

play_midi :: Transport.PlayControl -> Transport.ActivePlayers
    -> Interface.Interface -> Cmd.State -> Ui.State
    -> [LEvent.LEvent Midi.WriteMessage] -> IO ()
play_midi play_ctl players midi_interface cmd_state ui_state midi_msgs = do
    wdevs <- Interface.write_devices midi_interface
    mapM_ (Interface.connect_write_device midi_interface) (map fst wdevs)
    mvar <- MVar.newMVar ui_state
    Midi.Play.play (midi_state mvar) Nothing "tscore" midi_msgs Nothing
    where
    midi_state mvar = Midi.Play.State
        { _play_control = play_ctl
        , _players = players
        , _info = transport_info mvar
        , _im_end = Nothing
        }
    transport_info mvar = Transport.Info
        { info_send_status = \status -> print status -- TODO
        , info_midi_writer = Cmd.state_midi_writer cmd_state
        , info_midi_abort = Interface.abort midi_interface
        , info_get_current_time = Interface.now midi_interface
        -- This is unused by midi player, but Midi.Play wants it anyway
        , info_state = mvar
        }

print_midi_devices :: [(Midi.WriteDevice, [Midi.WriteDevice])]
    -> Map Midi.WriteDevice Midi.WriteDevice -> IO ()
print_midi_devices wdevs wdev_map =
    forM_ wdevs $ \(wdev, aliases) -> Text.IO.putStrLn $ Text.unwords $
        [ "  " <> pretty wdev
        , if null aliases then "" else pretty aliases
        , maybe "" (("<- "<>) . Text.intercalate ", " . map pretty) $
            Map.lookup wdev wdev_to_names
        ]
    where wdev_to_names = Maps.multimap $ map Tuple.swap $ Map.toList wdev_map

-- * derive

derive :: Ui.State -> Cmd.State -> BlockId
    -> (Vector.Vector Score.Event, [Log.Msg])
derive ui_state cmd_state block_id = (Cmd.perf_events perf, warns ++ logs)
    where
    (perf, logs) = Performance.derive ui_state cmd_state block_id
    warns = filter ((>=Log.Warn) . Log.msg_priority) (Cmd.perf_logs perf)

-- Derived from Solkattu.Play.derive_to_disk.
perform_im :: FilePath -> Cmd.State -> Ui.State -> Vector.Vector Score.Event
    -> BlockId -> IO ([Performance.Process], Vector.Vector Score.Event)
perform_im score_path cmd_state ui_state events block_id = do
    let im_config = Cmd.config_im (Cmd.state_config cmd_state)
        lookup_inst = either (const Nothing) Just
            . Cmd.state_lookup_instrument ui_state cmd_state
    (procs, non_im) <- Performance.evaluate_im im_config lookup_inst score_path
        0 1 block_id events
    return (procs, non_im)

play_im :: Maybe StreamAudio.Device -> FilePath -> Transport.ActivePlayers
    -> Transport.PlayControl -> BlockId -> RealTime
    -> Vector.Vector Score.Event -> Set ScoreT.Instrument
    -> [Performance.Process] -> IO ()
play_im mb_device score_path players play_ctl block_id start events
        im_instruments procs = do
    putStrLn $ "\nim render:"
    forM_ procs $ \(cmd, args) ->
        putStrLn $ unwords $ "%" : cmd : args
    ready <- MVar.newEmptyMVar
    Thread.startLogged "play_im" $ do
        ok <- Performance.wait_for_subprocesses
            (MVar.putMVar ready ())
            expected_instruments
            (Set.fromList procs)
        unless ok $ Log.warn "background render had a problem"
    Log.debug $ "wait for instruments: " <> pretty expected_instruments
    MVar.takeMVar ready
    let Transport.PlayControl quit = play_ctl
    let muted = mempty
    Transport.player_started players
    Thread.startLogged "stream_audio" $
        StreamAudio.play mb_device quit score_path block_id muted start
            `Exception.finally` Transport.player_stopped players
    return ()
    where
    -- This is a bit too tricky.  I want to make sure all instruments have
    -- the chunk at 'start' rendered.  But I can only wait for im instruments,
    -- or I'll be waiting forever, and then only the ones that are actually
    -- used.  I could filter on start, but since im emits empty chunks for
    -- instruments that haven't started, it should be harmless to wait for them
    -- all.
    expected_instruments = Vector.foldl' add mempty $
        Vector.filter ((`Set.member` im_instruments) . Score.event_instrument)
        events
    add = flip $ Set.insert . Score.event_instrument

get_im_instruments :: Ui.State -> Set ScoreT.Instrument
get_im_instruments = Set.fromList . map fst
    . filter (UiConfig.is_im_allocation . snd) . Map.toList
    . (Ui.config#UiConfig.allocations_map #$)

-- * load

type Error = Text

load_cmd_config :: Interface.Interface -> IO Cmd.Config
load_cmd_config midi_interface = do
    static_config <- Local.Config.load_static_config
    app_dir <- Path.get_app_dir
    save_dir <- Path.canonical $ Path.to_absolute app_dir App.Config.save_dir
    return $ StaticConfig.cmd_config app_dir save_dir midi_interface
        static_config (SaveGitT.User "user" "name")

load_score :: Cmd.Config -> Text -> IO (Either Error (Ui.State, Cmd.State))
load_score cmd_config source = Except.runExceptT $ do
    (ui_state, allocs) <- tryRight $ TScore.parse_score source
    -- TODO adjust starting line in error
    (builtins, aliases) <- tryRight . first ("parsing %ky: "<>)
        =<< liftIO (Ky.load ky_paths (Ui.config#UiConfig.ky #$ ui_state))
    let cmd_state =  DeriveSaved.add_library builtins aliases $
            Cmd.initial_state cmd_config
    allocs <- tryRight $ Instruments.update_ui
        (Cmd.get_lookup_backend cmd_state) allocs
        (Ui.config#UiConfig.allocations #$ ui_state)
    return (Ui.config#UiConfig.allocations #= allocs $ ui_state, cmd_state)
    where
    -- For now, I don't support ky import.
    ky_paths = []
