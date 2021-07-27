-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Standalone driver for tscore.
module Derive.TScore.TScoreMain where
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except

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
import qualified Util.Control as Control
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Processes as Processes
import qualified Util.Seq as Seq

import qualified App.Config as App.Config
import qualified App.Path as Path
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Ky as Ky
import qualified Cmd.Performance as Performance
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Simple as Simple

import qualified Derive.DeriveSaved as DeriveSaved
import qualified Derive.LEvent as LEvent
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
import qualified Perform.Midi.Patch as Midi.Patch
import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Transport as Transport

import qualified Synth.Shared.Config as Shared.Config
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
    Log.configure $ \state -> state { Log.state_priority = Log.Debug }
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

initialize_midi :: (Interface.Interface -> IO a) -> IO a
initialize_midi app = MidiDriver.initialize "tscore" (const False) $ \case
    Left err -> die $ "error initializing midi: " <> err
    Right midi_interface -> app =<< Interface.track_interface midi_interface

initialize_audio :: IO a -> IO a
initialize_audio = PortAudio.initialize

check_score :: FilePath -> IO ()
check_score fname = do
    source <- Text.IO.readFile fname
    case TScore.parse_score source of
        Left err -> Text.IO.putStrLn $ txt fname <> ": " <> err
        Right (ui_state, allocs) ->
            Text.IO.putStrLn $ Transform.show_stats ui_state

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
list_devices = initialize_audio $ initialize_midi $ \midi_interface -> do
    putStrLn "Audio devices:"
    default_dev <- PortAudio.getDefaultOutput
    audio_devs <- PortAudio.getOutputDevices
    forM_ (map PortAudio._name audio_devs) $ \dev ->
        putStrLn $ (if dev == PortAudio._name default_dev
            then "* " else "  ") <> show dev
    putStrLn "  \"sox\""
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
    dump_midi ui_state cmd_state events

dump_im :: Ui.State -> Cmd.State -> BlockId -> Vector.Vector Score.Event
    -> IO (Vector.Vector Score.Event)
dump_im ui_state cmd_state block_id events = do
    unless (null im_notes && null logs) $ do
        putStrLn "\n\tim events:"
        mapM_ Log.write logs
        mapM_ (Text.IO.putStrLn . Pretty.formatted) im_notes
    return midi_events
    where
    (im_events, midi_events) = Vector.partition is_im_event events
    (im_notes, logs) = LEvent.partition $
        Im.Convert.convert block_id lookup_inst $ Vector.toList im_events
    is_im_event =
        maybe False (is_im . Cmd.inst_instrument) . lookup_inst
            . Score.event_instrument
    is_im (Inst.Inst (Inst.Im {}) _) = True
    is_im _ = False
    lookup_inst = either (const Nothing) Just
        . Cmd.state_lookup_instrument ui_state cmd_state

dump_midi :: Ui.State -> Cmd.State -> Vector.Vector Score.Event -> IO ()
dump_midi ui_state cmd_state events =
    unless (null midi && null logs) $ do
        putStrLn "\n\tmidi msgs:"
        mapM_ Log.write logs
        mapM_ Pretty.pprint midi
    where
    (midi, logs) = LEvent.partition $
            DeriveSaved.perform_midi cmd_state ui_state events

play_score :: Maybe String -> FilePath -> IO ()
play_score mb_device fname = initialize_audio $ initialize_midi $
        \midi_interface -> do
    source <- Text.IO.readFile fname
    cmd_config <- load_cmd_config midi_interface
    (ui_state, cmd_state) <- either die return =<< load_score cmd_config source

    block_id <- maybe (die "no root block") return $
        Ui.config#UiConfig.root #$ ui_state
    let (events, logs) = derive ui_state cmd_state block_id
    mapM_ Log.write logs

    let start = 0 -- TODO from cmdline

    let score_path = fname
    (procs, events) <- perform_im score_path cmd_state ui_state events block_id
    play_ctl <- Transport.play_control
    players <- Transport.active_players
    unless (null procs) $ do
        putStrLn $ "\nim render:"
        forM_ procs $ \(cmd, args) ->
            putStrLn $ unwords $ "%" : cmd : args
        ready <- MVar.newEmptyMVar
        rendering <- Async.async $ do
            ok <- Performance.wait_for_subprocesses
                (MVar.putMVar ready ()) (Set.fromList procs)
            unless ok $ Log.warn "background render had a problem"
        putStrLn "wait for ready"
        MVar.takeMVar ready
        let Transport.PlayControl quit = play_ctl
        let muted = mempty
        Transport.player_started players
        Async.async $ StreamAudio.play quit score_path block_id muted start
            `Exception.finally` Transport.player_stopped players
        return ()

    -- TODO since I have to wait for im, I may have to bump MIDI forward.
    -- Of course they won't be very in sync anyway...
    unless (Vector.null events) $
        play_midi play_ctl players midi_interface cmd_state ui_state events

    Async.async $ do
        putStrLn "press return to stop player"
        _ <- IO.getLine
        Transport.stop_player play_ctl
    putStrLn "waiting for players to complete..."
    Transport.wait_player_stopped players
    putStrLn "done"

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

-- * im

-- TODO copy paste from Solkattu.Play, unify them

-- | This is analogous to 'Performance.watch_subprocesses', but notifies as
-- soon as all processes have rendered output.
--
-- It seems like overkill to watch multiple processes when there is always just
-- one (and can only be one, since I only load Sampler.PatchDb.synth below),
-- but that's what derive gives me.
watch_subprocesses :: MVar.MVar () -> Set Performance.Process -> IO ()
watch_subprocesses ready all_procs =
    Processes.multipleOutput (Set.toList all_procs) $ \chan ->
        Control.loop1 (all_procs, Set.empty) $ \loop (procs, started) -> if
            | Set.null procs -> MVar.tryPutMVar ready () >> return ()
            | otherwise -> do
                ((cmd, args), out) <- Chan.readChan chan
                loop =<< process procs started (cmd, args) out
    where
    process procs started (cmd, args) = \case
        Processes.Stderr line -> do
            -- Only show Log.Warn and above.
            -- TODO pass LOG_CONFIG?
            when ("---" `Text.isPrefixOf` line) $
                put line
            return (procs, started)
        Processes.Stdout line -> case Shared.Config.parseMessage line of
            Just (Shared.Config.Message
                    { Shared.Config._payload = Shared.Config.WaveformsCompleted chunks })
                | 0 `elem` chunks -> do
                    -- I can start playing when I see the first progress for
                    -- each process, and for each instrument.  Since I only
                    -- have one instrument the first suffices.
                    let started2 = Set.insert (cmd, args) started
                    when (started2 == all_procs) $
                        void $ MVar.tryPutMVar ready ()
                    return (procs, started2)
                | otherwise -> return (procs, started)
            Just (Shared.Config.Message { Shared.Config._payload = Shared.Config.RenderingRange {} })
                -> return (procs, started)
            _ -> put ("?: " <> line) >> return (procs, started)
        Processes.Exit code -> do
            when (code /= Processes.ExitCode 0) $
                Log.warn $ "subprocess " <> txt cmd <> " "
                    <> showt args <> " returned " <> showt code
            return (Set.delete (cmd, args) procs, started)
    -- These get called concurrently, so avoid jumbled output.
    put line = Log.with_stdio_lock $ Text.IO.hPutStrLn IO.stdout line

-- * midi

play_midi :: Transport.PlayControl -> Transport.ActivePlayers
    -> Interface.Interface -> Cmd.State -> Ui.State
    -> Vector.Vector Score.Event -> IO ()
play_midi play_ctl players midi_interface cmd_state ui_state events = do
    wdevs <- Interface.write_devices midi_interface
    mapM_ (Interface.connect_write_device midi_interface) (map fst wdevs)
    let midi = DeriveSaved.perform_midi cmd_state ui_state events
    mvar <- MVar.newMVar ui_state
    let midi_state = Midi.Play.State
            { _play_control = play_ctl
            , _players = players
            , _info = transport_info mvar
            , _im_end = Nothing
            }
    Midi.Play.play midi_state Nothing "tscore" midi Nothing
    where
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

-- * load

type Error = Text

load_cmd_config :: Interface.Interface -> IO Cmd.Config
load_cmd_config midi_interface = do
    static_config <- Local.Config.load_static_config
    app_dir <- Path.get_app_dir
    save_dir <- Path.canonical $ Path.to_absolute app_dir App.Config.save_dir
    return $ StaticConfig.cmd_config app_dir save_dir midi_interface
        static_config (SaveGit.User "user" "name")

load_score :: Cmd.Config -> Text -> IO (Either Error (Ui.State, Cmd.State))
load_score cmd_config source = Except.runExceptT $ do
    (ui_state, instruments) <- tryRight $ TScore.parse_score source
    -- TODO adjust starting line in error
    (builtins, aliases) <- tryRight . first ("parsing %ky: "<>)
        =<< liftIO (Ky.load ky_paths ui_state)
    let cmd_state =  DeriveSaved.add_library builtins aliases $
            Cmd.initial_state cmd_config
    ui_state <- tryRight $ first pretty $ Ui.exec ui_state $
        forM_ instruments $ uncurry (allocate cmd_config) . convert_allocation
    return (ui_state, cmd_state)
    where
    -- For now, I don't support ky import.
    ky_paths = []

-- | Like 'Cmd.allocate', but doesn't require Cmd.M.
allocate :: Ui.M m => Cmd.Config -> ScoreT.Instrument -> UiConfig.Allocation
    -> m ()
allocate cmd_config score_inst alloc = do
    let qualified = UiConfig.alloc_qualified alloc
    inst <- Ui.require ("instrument not in db: " <> pretty qualified) $
        Cmd.state_lookup_qualified cmd_config qualified
    allocs <- Ui.config#UiConfig.allocations <#> Ui.get
    allocs <- Ui.require_right id $
        UiConfig.allocate (Inst.inst_backend inst) score_inst alloc allocs
    Ui.modify_config $ UiConfig.allocations #= allocs

convert_allocation :: T.Allocation -> (ScoreT.Instrument, UiConfig.Allocation)
convert_allocation (T.Allocation inst qual backend) =
    ( ScoreT.Instrument inst
    , UiConfig.allocation qual $ case backend of
        T.Im -> UiConfig.Im
        T.Midi chans -> UiConfig.Midi $
            Midi.Patch.config (map (, Nothing) chans)
    )
