-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This module manages the performance of music, specifically the creation
    of performance threads.

    Performance is relative to a toplevel block, so each block has its own set
    of caches.  Since performance is lazy, a separate thread will force it
    asynchronously.
-}
module Cmd.Performance (
    SendStatus, update_performance, derive_blocks, performance, derive
    , Process, evaluate_im
    , wait_for_subprocesses
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Monad.State.Strict as Monad.State
import qualified Data.HashSet as HashSet
import qualified Data.List as List
import qualified Data.Map.Lazy as Map.Lazy
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as Vector

import qualified Streaming.Prelude as S
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.IO as IO

import qualified Util.Control as Control
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Processes as Processes
import qualified Util.Thread as Thread
import qualified Util.Vector

import qualified App.Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Instrument.Inst as Inst
import qualified Perform.Im.Convert as Im.Convert
import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport

import qualified Synth.ImGc as ImGc
import qualified Synth.Shared.Config as Config
import qualified Ui.Block as Block
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


type SendStatus = BlockId -> Msg.DeriveStatus -> IO ()
type StateM = Monad.State.StateT Cmd.State IO ()

{- | Update the performances by rederiving if necessary.  This means figuring
    out ScoreDamage, and if there has been damage, killing any in-progress
    derivation and starting derivation.  This updates performances for the root
    block and all visible blocks.

    The majority of the calls here will bring neither score damage nor
    a changed view id, and thus this will do nothing.

    This is tricky, and I've gotten it wrong in the past, so here's a detailed
    description:

    Merge ui damage with each perf's damage.  Then for each perf, if it's
    'Cmd.state_current_performance' has damage, kill its thread, and remove its
    entry in 'Cmd.state_performance_threads'.  The lack of a thread entry,
    whether because was removed or never existed, means that a block should be
    rederived.  Derivation creates a new 'Cmd.Performance' and an evaluate
    thread, and puts them into 'Cmd.state_current_performance' and
    'Cmd.state_performance_threads' respectively, but due to laziness, no actual
    derivation happens unless someone (like play) happens to look at the
    performance.  This all happens synchronously, so the next time
    'update_performance' is called, it sees a nice clean new Performance with
    no damage.

    Meanwhile, the evaluate thread asynchronously waits for a bit, then
    forces the contents of the Performance, and then sends it back to the
    responder so it can stash it in 'Cmd.state_performance'.  If a new change
    comes in while it's waiting it'll get killed off, and the out-of-date
    derivation will never happen.  Yay for laziness!
-}
update_performance :: SendStatus -> Ui.State -> Cmd.State
    -> Derive.ScoreDamage -> IO Cmd.State
update_performance send_status ui_state cmd_state damage =
    -- The update will be modifying Cmd.State, especially PlayState.
    Monad.State.execStateT (run_update send_status ui_state)
        (insert_damage damage cmd_state)

run_update :: SendStatus -> Ui.State -> StateM
run_update send_status ui_state = do
    kill_threads
    mapM_ (try_generate_performance send_status ui_state)
        (derive_blocks ui_state)

-- | Which blocks should get derived?
derive_blocks :: Ui.State -> Set BlockId
derive_blocks ui_state = Set.fromList $ maybe id (:) root_id visible
    where
    root_id = UiConfig.config_root (Ui.state_config ui_state)
    visible = map Block.view_block $ Map.elems $ Ui.state_views ui_state

try_generate_performance :: SendStatus -> Ui.State -> BlockId -> StateM
try_generate_performance send_status ui_state block_id = do
    state <- Monad.State.get
    when (needs_generate state block_id) $
        generate_performance ui_state (derive_wait state block_id)
            send_status block_id

-- | Theoretically I should be able to do away with the wait, but in practice
-- deriving constantly causes UI latency.
derive_wait :: Cmd.State -> BlockId -> Thread.Seconds
derive_wait cmd_state block_id
    | block_id `Set.member` Cmd.state_derive_immediately cmd_state = 0
    | otherwise = App.Config.default_derive_wait

-- | Since caches are stored per-performance, score damage is also
-- per-performance.  It accumulates in an existing performance and is cleared
-- when a new performance is created from the old one.
insert_damage :: Derive.ScoreDamage -> Cmd.State -> Cmd.State
insert_damage damage
    | damage == mempty = id
    | otherwise = modify_play_state $ \st -> st
        -- Damage update is tricky.  Damage in 'Cmd.state_current_performance'
        -- is a signal that the performance for that block is out of date and
        -- needs to be updated.  Technically all I need is a Bool since it just
        -- checks (damage /= mempty).  But this signal has to go in the current
        -- performance, since it's updated (and hence the damage is cleared)
        -- synchronously, and otherwise I'd get stuck in a loop killing and
        -- starting new derivations.
        --
        -- However, the derivation is relative to 'Cmd.state_performance', so
        -- the damage is also relative to it.  So this damage is actually used
        -- for derivation, not as a out-of-date flag.  When
        -- state_current_performance is promoted to state_performance, the
        -- damage is also cleared.
        { Cmd.state_current_performance =
            update <$> Cmd.state_current_performance st
        , Cmd.state_performance = update <$> Cmd.state_performance st
        }
    where
    update perf
        | dependency_damaged perf =
            let !accum = damage <> Cmd.perf_damage perf
            in perf { Cmd.perf_damage = accum }
        | otherwise = perf
    dependency_damaged perf = not $ Set.disjoint damaged_blocks deps
        where Derive.BlockDeps deps = Cmd.perf_block_deps perf
    damaged_blocks =
        Derive.sdamage_track_blocks damage <> Derive.sdamage_blocks damage

-- | Kill all performance threads with damage.  If they are still deriving
-- they're now out of date and should stop.  Whether or not they finished
-- deriving, this will remove them from 'Cmd.state_performance_threads',
-- which will cause them to rederive.
kill_threads :: StateM
kill_threads = do
    play_state <- Monad.State.gets Cmd.state_play
    let threads = Cmd.state_performance_threads play_state
        perfs = Cmd.state_current_performance play_state
        with_damage =
            [ block_id | (block_id, perf) <- Map.toList perfs
            , Cmd.perf_damage perf /= mempty
            ]
        -- Kill threads for damaged blocks, but also ones with no performance
        -- at all.  This is so Cmd.invalidate_performances can force a
        -- re-generate by clearing out state_current_performance, and doesn't
        -- need to be in IO to immediately kill the threads.
        obsolete bid = Map.notMember bid perfs || bid `elem` with_damage
        kill = filter (obsolete . fst) $ Map.toList threads
    liftIO $ mapM_ (Cmd.kill_thread . snd) kill
    Monad.State.modify $ modify_play_state $ const $ play_state
        { Cmd.state_performance_threads = Maps.deleteKeys (map fst kill)
            (Cmd.state_performance_threads play_state)
        }


-- * performance evaluation

-- | True if this BlockId should be regenerated.  This happens if there is
-- no performance, which means either there never was one, or it was deleted
-- by 'kill_threads' thanks to ScoreDamage.
--
-- I use 'Cmd.state_performance_threads' and not
-- 'Cmd.state_current_performance', because the thread is filled in
-- synchronously, while the current performance is filled in later.
needs_generate :: Cmd.State -> BlockId -> Bool
needs_generate state block_id = not (Map.member block_id perfs)
    where perfs = Cmd.state_performance_threads $ Cmd.state_play state

-- | Start a new performance thread.
--
-- Pull previous caches from the existing performance, if any.  Use them to
-- generate a new performance, kick off a thread for it, and insert the new
-- thread into 'Cmd.state_performance_threads' and performance into
-- 'Cmd.state_current_performance'.  It will be promoted to
-- 'Cmd.state_performance' when 'evaluate_performance' completes.
generate_performance :: Ui.State -> Thread.Seconds -> SendStatus -> BlockId
    -> StateM
generate_performance ui_state wait send_status block_id = do
    cmd_state <- Monad.State.get
    let (perf, logs) = derive ui_state cmd_state block_id
    mapM_ Log.write logs
    thread_id <- liftIO $ Thread.asyncLogged ("perf:" <> prettys block_id) $ do
        let allocs = Ui.config#UiConfig.allocations #$ ui_state
            im_config = Cmd.config_im (Cmd.state_config cmd_state)
        let lookup_inst = either (const Nothing) Just
                . Cmd.state_lookup_instrument ui_state cmd_state
        evaluate_performance
            (if im_allocated allocs then Just im_config else Nothing)
            lookup_inst wait send_status (Cmd.score_path cmd_state)
            (Cmd.state_play_multiplier (Cmd.state_play cmd_state)) block_id perf
    Monad.State.modify $ modify_play_state $ \st -> st
        { Cmd.state_performance_threads = Map.insert block_id
            (Cmd.Thread thread_id) (Cmd.state_performance_threads st)
        , Cmd.state_current_performance = Map.insert block_id
            perf (Cmd.state_current_performance st)
        }
    -- If the derivation somehow failed, then the old performance will remain,
    -- and since there is no thread, this will try again the next time around.

{-# SCC derive #-}
derive :: Ui.State -> Cmd.State -> BlockId -> (Cmd.Performance, [Log.Msg])
derive ui_state cmd_state block_id = (perf, logs)
    where
    perf = case cmd_result of
        Left err -> broken_performance $
            "derivation for " <> showt block_id <> " failed: " <> pretty err
        Right (derive_result, _, _) -> case derive_result of
            Nothing -> broken_performance $
                "derivation for " <> showt block_id <> " aborted"
            Just result -> performance ui_state result
    -- The previous cache comes from the fully evaluated performance, since
    -- otherwise there's no point killing the 'evaluate_performance' thread if
    -- the cache makes all derivation serialized.
    prev_cache = maybe mempty Cmd.perf_derive_cache $ Map.lookup block_id $
        Cmd.state_performance $ Cmd.state_play cmd_state
    -- The damage also comes from the current performance, since that's where
    -- the performance is deriving from.
    damage = maybe mempty Cmd.perf_damage $ Map.lookup block_id $
        Cmd.state_performance $ Cmd.state_play cmd_state
    (_state, _midi, logs, cmd_result) = Cmd.run_id ui_state cmd_state $
        PlayUtil.derive_block prev_cache damage block_id

evaluate_performance :: Maybe Config.Config
    -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> Thread.Seconds -> SendStatus -> FilePath -> RealTime -> BlockId
    -> Cmd.Performance -> IO ()
evaluate_performance im_config lookup_inst wait send_status score_path
        play_multiplier block_id perf = do
    send_status block_id Msg.OutOfDate
    Thread.delay wait
    send_status block_id Msg.Deriving
    -- I just force the logs here, and wait for a play to actually write them.
    ((), metric) <- Thread.timeAction $ return $! Msg.force_performance perf
    forM_ (check_dummy lookup_inst (Msg.perf_events perf)) $ \(event, msg) ->
        Log.warn $ "note with dummy instrument: " <> Score.short_event event
            <> ": " <> msg
    when (Thread.metricWall metric > 1) $
        Log.notice $ "derived " <> showt block_id <> " in "
            <> Thread.showMetric metric
    -- This should match the adjust0 in 'Cmd.Play.start_adjustment'.
    let adjust0 = case Vector.toList (Cmd.perf_events perf) of
            event : _ -> max (- Score.event_start event) 0
            _ -> 0
    (procs, events) <- case im_config of
        Nothing -> return ([], Cmd.perf_events perf)
        Just config -> evaluate_im config lookup_inst score_path
            adjust0 play_multiplier block_id (Cmd.perf_events perf)
    send_status block_id $ Msg.DeriveComplete
        (perf { Cmd.perf_events = events })
        (if null procs then Msg.ImUnnecessary else Msg.ImStarted)
    ok <- case im_config of
        Just config -> watch_subprocesses block_id
            (make_status (Cmd.perf_inv_tempo perf)
                (state_wants_waveform (Cmd.perf_ui_state perf))
                (Config.imDir config) score_path adjust0 play_multiplier)
            (send_status block_id)
            (Set.fromList procs)
        Nothing -> return True
    unless (null procs) $ do
        stats <- case (ok, im_config) of
            (True, Just config) -> fmap Just $ im_gc $
                Config.outputDirectory (Config.imDir config) score_path block_id
            _ -> return Nothing
        send_status block_id $ Msg.ImStatus block_id Set.empty $
            Msg.ImComplete (not ok) stats

im_gc :: FilePath -> IO ImGc.Stats
im_gc output_dir = do
    stats <- ImGc.gc output_dir
    when (ImGc._deletedFiles stats > 0) $
        Log.notice $ ImGc.showStats stats
    return stats

-- | Return events with a 'Inst.Dummy' backend.  These shouldn't have made it
-- through to actual performance.
check_dummy :: (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> Vector.Vector Score.Event -> [(Score.Event, Text)]
check_dummy lookup_inst = snd . Vector.foldl' go (HashSet.empty, [])
    where
    go (seen, warns) event
        | HashSet.member inst seen = (seen, warns)
        | otherwise =
            ( HashSet.insert inst seen
            , maybe warns (\msg -> (event, msg) : warns) (is_dummy inst)
            -- , if is_dummy inst then event : warns else warns
            )
        where
        inst = Score.event_instrument event
    is_dummy inst = case Cmd.inst_backend <$> lookup_inst inst of
        Just (Cmd.Dummy msg) -> Just msg
        _ -> Nothing

state_wants_waveform :: Ui.State -> TrackId -> Bool
state_wants_waveform state track_id = maybe False Track.track_waveform $
    Map.lookup track_id (Ui.state_tracks state)

type Process = (FilePath, [String])

-- | Watch subprocesses and convert their msgs to Msg.DeriveStatus.
watch_subprocesses :: BlockId -> (Config.Message -> ImStatus)
    -> (Msg.DeriveStatus -> IO ()) -> Set Process -> IO Bool
watch_subprocesses root_block_id make_status send_status procs =
    stream_subprocesses procs $ S.mapM_ watch
    where
    watch ((cmd, args), ProcessExit code)
        | code /= Processes.ExitCode 0 = Log.warn $ "subprocess failed with "
            <> showt code <> ": " <> Text.unwords (map txt (cmd : args))
        | otherwise = return ()
    watch (_, Message msg)
        -- This is an update for a child block.  I only display progress
        -- for each block as its own toplevel.  If a block is child of
        -- another, I can see its prorgess in the parent.
        | Config._blockId msg /= root_block_id = return ()
        | otherwise = case make_status msg of
            ImStatus status -> send_status =<< resolve_waveform_links status
            ImWarn msg -> Log.write msg
            ImFail msg -> Log.write msg
            ImNothing -> return ()

-- | Like 'watch_subprocesses', but notify the callback as soon as
-- they have all rendered enough audio.
wait_for_subprocesses :: IO () -> Set ScoreT.Instrument -> Set Process
    -> IO Bool
wait_for_subprocesses ready expected_instruments procs =
    stream_subprocesses procs $ s_mapAccumL_ check (Set.empty, Set.empty)
    where
    check (started, insts) ((cmd, args), ProcessExit code) = do
        unless (code == Processes.ExitCode 0) $
            Log.warn $ "subprocess failed with "
                <> showt code <> ": " <> Text.unwords (map txt (cmd : args))
        return (started, insts)
    check (started, insts) (proc, Message msg) = case Config._payload msg of
        Config.WaveformsCompleted chunks
            | 0 `elem` chunks -> do
                -- TODO compute chunk for a specific time, not always 0
                let started2 = Set.insert proc started
                    insts2 = Set.insert (Config._instrument msg) insts
                Log.debug $ "started: " <> pretty started2
                    <> ", insts: " <> pretty insts2
                when (started2 == procs && insts2 == expected_instruments) ready
                return (started2, insts2)
            | otherwise -> ignore
        Config.Failure err -> Log.warn err >> ignore
        Config.Warn stack msg -> Log.warn_stack stack msg >> ignore
        Config.RenderingRange {} -> ignore
        where ignore = return (started, insts)

data Progress = ProcessExit Processes.Exit | Message Config.Message

-- | Watch each subprocess, return when they all exit.  The stream returns
-- False if there was some failure along the way.
--
-- This is a bracket and doesn't return the stream because I want to kill the
-- subprocesses when leaving the scope.
stream_subprocesses :: Set Process
    -> (S.Stream (S.Of (Process, Progress)) IO Bool -> IO a) -> IO a
stream_subprocesses procs action
    | Set.null procs = action $ return False
    | otherwise = Processes.multipleOutput (Set.toList procs) $ \chan ->
        action $ Control.loop1 (procs, True) $ \loop (procs, ok) -> if
            | Set.null procs -> return ok
            | otherwise -> do
                (proc, out) <- liftIO $ Chan.readChan chan
                loop =<< process procs ok proc out
    where
    process procs ok proc = \case
        Processes.Stderr line -> put line >> return (procs, ok)
        Processes.Stdout line -> do
            ok1 <- progress proc line
            return (procs, ok && ok1)
        Processes.Exit code -> do
            S.yield (proc, ProcessExit code)
            return (Set.delete proc procs, ok && code == Processes.ExitCode 0)
    progress proc line = case Config.parseMessage line of
        Nothing -> do
            put $ "couldn't parse: " <> line
            return False
        Just msg -> do
            S.yield (proc, Message msg)
            return $ case Config._payload msg of
                Config.Failure {} -> False
                _ -> True
    -- These get called concurrently, so avoid jumbled output.
    put line = liftIO $ Log.with_stdio_lock $ Text.IO.hPutStrLn IO.stdout line

data ImStatus =
    ImStatus Msg.DeriveStatus | ImWarn Log.Msg | ImFail Log.Msg | ImNothing
    deriving (Show)

-- | Convert the status output from an im subprocess to an 'ImStatus'.
make_status :: Transport.InverseTempoFunction -> (TrackId -> Bool) -> FilePath
    -> FilePath -> RealTime -> RealTime -> Config.Message -> ImStatus
make_status inv_tempo wants_waveform im_dir score_path adjust0 play_multiplier
        (Config.Message block_id track_ids instrument payload) =
    case payload of
        Config.RenderingRange start end ->
            status $ Msg.ImRenderingRange instrument start end
        Config.WaveformsCompleted chunknums
            | Set.null wanted_track_ids -> ImNothing
            | otherwise -> case mapM make_waveform chunknums of
                Right waveforms -> status $ Msg.ImWaveformsCompleted waveforms
                Left err -> ImWarn $ Log.msg Log.Warn Nothing $
                    "to_score for " <> pretty chunknums <> ": " <> err
        Config.Warn stack err -> ImWarn $ Log.msg Log.Warn (Just stack) $
            "im instrument " <> pretty instrument <> ": " <> err
        Config.Failure err -> ImFail $ Log.msg Log.Warn Nothing $
            "im failure: " <> pretty block_id <> ": " <> pretty track_ids
            <> ": " <> err
    where
    status = ImStatus . Msg.ImStatus block_id wanted_track_ids

    wanted_track_ids = Set.filter wants_waveform track_ids
    make_waveform chunknum = do
        start <- to_score $ time_at chunknum
        end <- to_score $ time_at (chunknum+1)
        adjust0 <- to_score adjust0
        -- ratio=2 means half as long.  So 2s -> 1t is 2/1
        let ratio = fromIntegral Config.chunkSeconds
                / ScoreTime.to_double (end - start)
        -- Debug.tracepM "start, ratio"
        --     ((time_at chunknum, time_at (chunknum+1)), (start, end), ratio)
        return $ Track.WaveformChunk
            { _filename = Config.chunkPath im_dir score_path block_id
                instrument chunknum
            , _chunknum = chunknum
            , _start = start * RealTime.to_score play_multiplier - adjust0
            , _ratios = [ratio / RealTime.to_seconds play_multiplier]
            }
    time_at chunknum = RealTime.seconds $
        fromIntegral $ chunknum * Config.chunkSeconds
    -- If this fails, it means I don't have any tempo info for this
    -- (block_id, track_id), which likely means the block or track failed
    -- to derive, at which point I shouldn't have gotten any notes from it.
    -- So this shouldn't happen.
    to_score t = tryJust ("no ScoreTime for " <> pretty t) $
        real_to_score inv_tempo block_id track_ids t

-- | Resolve symlinks in 'Track._filename's.  The PeakCache uses the filename
-- as a key, so I can get false hits if I use the symlinks, which all look like
-- 000.wav.
--
-- I don't do this in 'make_status' because that would put the whole thing into
-- IO.
resolve_waveform_links :: Msg.DeriveStatus -> IO Msg.DeriveStatus
resolve_waveform_links (Msg.ImStatus block_id track_ids
        (Msg.ImWaveformsCompleted waveforms)) = do

    -- resolved <- mapM (Directory.getSymbolicLinkTarget . Track._filename)
    --     waveforms
    -- when (any (not . ("checkpoint/" `List.isPrefixOf`)) resolved) $ do
    --     Log.error $ "bad symlink in: " <> pretty
    --         (zip (map Track._filename waveforms) resolved)
    --     let dir = FilePath.takeDirectory $ Track._filename (head waveforms)
    --     Process.callProcess "ls" ["-l", dir]

    fns <- mapM (resolve_link . Track._filename) waveforms
    -- I got a directory in there once and don't know why...
    when (any (not . (".wav" `List.isSuffixOf`)) fns) $ do
        Log.error $ "waveforms resolved to non-wav: "
            <> pretty (zip (map Track._filename waveforms) fns)
        fns2 <- mapM (resolve_link . Track._filename) waveforms
        Log.error $ "tried again:"
            <> pretty (zip (map Track._filename waveforms) fns2)

    return $ Msg.ImStatus block_id track_ids $ Msg.ImWaveformsCompleted
        [wave { Track._filename = fn } | (fn, wave) <- zip fns waveforms]
resolve_waveform_links status = return status

resolve_link :: FilePath -> IO FilePath
resolve_link fname = do
    dest <- Directory.getSymbolicLinkTarget fname
    dest <- if "checkpoint/" `List.isPrefixOf` dest
            && ".wav" `List.isSuffixOf` dest
        then return dest
        else do
            dest2 <- Directory.getSymbolicLinkTarget fname
            Log.error $ "bad symlink: " <> showt fname <> " -> " <> showt dest
                <> ", next time: " <> showt dest2
            return dest2
    return $ FilePath.takeDirectory fname </> dest

-- resolve_link :: FilePath -> IO FilePath
-- resolve_link fname =
--     (FilePath.takeDirectory fname </>) <$> Directory.getSymbolicLinkTarget fname

real_to_score :: Transport.InverseTempoFunction -> BlockId -> Set TrackId
    -> RealTime -> Maybe ScoreTime
real_to_score inv_tempo block_id track_ids =
    fmap snd . List.find ((`Set.member` track_ids) . fst) <=< lookup block_id
        . inv_tempo Transport.NoStop

-- | If there are im events, serialize them and return a Processes to render
-- them, and the non-im events.
evaluate_im :: Config.Config
    -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> FilePath -> RealTime -> RealTime -> BlockId -> Vector.Vector Score.Event
    -> IO ([Process], Vector.Vector Score.Event)
evaluate_im config lookup_inst score_path adjust0 play_multiplier block_id
        events = do
    procs <- Maybe.catMaybes <$> mapM write_notes by_synth
    Config.clearUnusedInstruments output_dir instruments
    return (procs, fromMaybe mempty $ lookup Nothing by_synth)
    where
    instruments = Vector.foldl' add mempty events
        where add = flip $ HashSet.insert . Score.event_instrument
    by_synth = Util.Vector.partition_on im_synth events
    im_synth event = case lookup_inst (Score.event_instrument event) of
        Just inst -> case Cmd.inst_instrument inst of
            Inst.Inst (Inst.Im {}) _ -> Just (Cmd.inst_synth inst)
            _ -> Nothing
        Nothing -> Nothing

    output_dir = Config.outputDirectory (Config.imDir config) score_path
        block_id
    write_notes (Just synth_name, events) = do
        case Map.lookup synth_name (Config.synths config) of
            Just synth -> do
                let notes_file = Config.notesFilename (Config.imDir config)
                        score_path block_id synth
                -- I used to get the changed flag out of Im.Convert.write and
                -- skip the subprocess if it hadn't changed.  But that gets in
                -- the way of getting waveforms on the first run (assuming the
                -- notes haven't been touched since).  In any case, I'm now
                -- more careful in insert_damage, so we shouldn't even get here
                -- unless there was damage on the block, and if there was but
                -- notes haven't changed, the im synth should hit its cache.
                Im.Convert.write adjust0 play_multiplier block_id
                    lookup_inst notes_file events
                let binary = Config.binary synth
                return $ if null binary
                    then Nothing
                    else Just (binary, ["--progress", notes_file, output_dir])
            Nothing -> do
                Log.warn $ "unknown im synth " <> showt synth_name <> " with "
                    <> showt (Vector.length events) <> " events"
                return Nothing
    write_notes (Nothing, _) = return Nothing

-- | If there are no UiConfig.Im instruments, then I don't need to bother to
-- partition out its events.  However, it means I won't get errors if there
-- happen to be any, but I'll worry about that if it becomes a problem.
im_allocated :: UiConfig.Allocations -> Bool
im_allocated (UiConfig.Allocations allocs) =
    any ((==UiConfig.Im) . UiConfig.alloc_backend) (Map.elems allocs)

-- | Make a broken performance with just an error msg.  This ensures that
-- the msg is logged when you try to play, but will still suppress further
-- performance, so you don't get a million msgs.
broken_performance :: Text -> Cmd.Performance
broken_performance msg = Cmd.Performance
    { perf_derive_cache = mempty
    , perf_events = mempty
    , perf_logs = [Log.msg Log.Warn Nothing msg]
    , perf_logs_written = False
    , perf_track_dynamic = mempty
    , perf_integrated = mempty
    , perf_damage = mempty
    , perf_warps = mempty
    , perf_track_signals = mempty
    , perf_block_deps = mempty
    , perf_track_instruments = mempty
    , perf_ui_state = Ui.empty
    }

-- | Constructor for 'Cmd.Performance'.
performance :: Ui.State -> Derive.Result -> Cmd.Performance
performance state result = Cmd.Performance
    { perf_derive_cache = Derive.r_cache result
    , perf_events = vevents
    , perf_logs = logs
    , perf_logs_written = False
    , perf_track_dynamic = Derive.r_track_dynamic result
    , perf_integrated = Derive.r_integrated result
    , perf_damage = mempty
    , perf_warps = Derive.r_track_warps result
    , perf_track_signals = Derive.r_track_signals result
    , perf_block_deps = Derive.collect_block_deps $ Derive.state_collect $
        Derive.r_state result
    , perf_track_instruments = track_instruments
        vevents (Map.keys (Ui.state_tracks state))
    , perf_ui_state = state
    }
    where
    vevents = Vector.fromList events
    (events, logs) = Stream.partition (Derive.r_events result)

modify_play_state :: (Cmd.PlayState -> Cmd.PlayState) -> Cmd.State -> Cmd.State
modify_play_state modify state =
    state { Cmd.state_play = modify (Cmd.state_play state) }

track_instruments :: Vector.Vector Score.Event -> [TrackId]
    -> Map TrackId (Set ScoreT.Instrument)
track_instruments events = Map.Lazy.fromAscList . Lists.keyOnSnd instruments_of
    -- fromAscList should be safe since Map.keys returns in order.
    where
    -- Go by TrackId, so I can make each Map value lazy, since I will only
    -- even need the instruments if I do an im mute on the track.
    instruments_of track_id = Vector.foldl' (get track_id) mempty events
    get track_id seen event
        | track_id `elem` track_ids =
            Set.insert (Score.event_instrument event) seen
        | otherwise = seen
        where track_ids = Stack.track_ids_of (Score.event_stack event)

-- * util

-- | Like `S.mapM_` but with state.
s_mapAccumL_ :: Monad m => (st -> a -> m st) -> st -> S.Stream (S.Of a) m r
    -> m r
s_mapAccumL_ f = go
    where
    go st xs = S.next xs >>= \case
        Left r -> return r
        Right (x, xs) -> do
            st <- f st x
            go st xs
