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
    SendStatus, updatePerformance, deriveBlocks, performance, derive
    , Process, evaluateIm
    , waitForSubprocesses
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
import qualified Util.Streams as Streams
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
updatePerformance :: SendStatus -> Ui.State -> Cmd.State
    -> Derive.ScoreDamage -> IO Cmd.State
updatePerformance sendStatus uiState cmdState damage =
    -- The update will be modifying Cmd.State, especially PlayState.
    Monad.State.execStateT (runUpdate sendStatus uiState)
        (insertDamage damage cmdState)

runUpdate :: SendStatus -> Ui.State -> StateM
runUpdate sendStatus uiState = do
    killThreads
    mapM_ (tryGeneratePerformance sendStatus uiState)
        (deriveBlocks uiState)

-- | Which blocks should get derived?
deriveBlocks :: Ui.State -> Set BlockId
deriveBlocks uiState = Set.fromList $ maybe id (:) rootId visible
    where
    rootId = UiConfig.configRoot (Ui.stateConfig uiState)
    visible = map Block.viewBlock $ Map.elems $ Ui.stateViews uiState

tryGeneratePerformance :: SendStatus -> Ui.State -> BlockId -> StateM
tryGeneratePerformance sendStatus uiState blockId = do
    state <- Monad.State.get
    when (needsGenerate state blockId) $
        generatePerformance uiState (deriveWait state blockId)
            sendStatus blockId

-- | Theoretically I should be able to do away with the wait, but in practice
-- deriving constantly causes UI latency.
deriveWait :: Cmd.State -> BlockId -> Thread.Seconds
deriveWait cmdState blockId
    | blockId `Set.member` Cmd.stateDeriveImmediately cmdState = 0
    | otherwise = App.Config.defaultDeriveWait

-- | Since caches are stored per-performance, score damage is also
-- per-performance.  It accumulates in an existing performance and is cleared
-- when a new performance is created from the old one.
insertDamage :: Derive.ScoreDamage -> Cmd.State -> Cmd.State
insertDamage damage
    | damage == mempty = id
    | otherwise = modifyPlayState $ \st -> st
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
        { Cmd.stateCurrentPerformance =
            update <$> Cmd.stateCurrentPerformance st
        , Cmd.statePerformance = update <$> Cmd.statePerformance st
        }
    where
    update perf
        | dependencyDamaged perf =
            let !accum = damage <> Cmd.perfDamage perf
            in perf { Cmd.perfDamage = accum }
        | otherwise = perf
    dependencyDamaged perf = not $ Set.disjoint damagedBlocks deps
        where Derive.BlockDeps deps = Cmd.perfBlockDeps perf
    damagedBlocks =
        Derive.sdamageTrackBlocks damage <> Derive.sdamageBlocks damage

-- | Kill all performance threads with damage.  If they are still deriving
-- they're now out of date and should stop.  Whether or not they finished
-- deriving, this will remove them from 'Cmd.state_performance_threads',
-- which will cause them to rederive.
killThreads :: StateM
killThreads = do
    playState <- Monad.State.gets Cmd.statePlay
    let threads = Cmd.statePerformanceThreads playState
        perfs = Cmd.stateCurrentPerformance playState
        withDamage =
            [ blockId | (blockId, perf) <- Map.toList perfs
            , Cmd.perfDamage perf /= mempty
            ]
        -- Kill threads for damaged blocks, but also ones with no performance
        -- at all.  This is so Cmd.invalidate_performances can force a
        -- re-generate by clearing out state_current_performance, and doesn't
        -- need to be in IO to immediately kill the threads.
        obsolete bid = Map.notMember bid perfs || bid `elem` withDamage
        kill = filter (obsolete . fst) $ Map.toList threads
    liftIO $ mapM_ (Cmd.killThread . snd) kill
    Monad.State.modify $ modifyPlayState $ const $ playState
        { Cmd.statePerformanceThreads = Maps.deleteKeys (map fst kill)
            (Cmd.statePerformanceThreads playState)
        }


-- * performance evaluation

-- | True if this BlockId should be regenerated.  This happens if there is
-- no performance, which means either there never was one, or it was deleted
-- by 'kill_threads' thanks to ScoreDamage.
--
-- I use 'Cmd.state_performance_threads' and not
-- 'Cmd.state_current_performance', because the thread is filled in
-- synchronously, while the current performance is filled in later.
needsGenerate :: Cmd.State -> BlockId -> Bool
needsGenerate state blockId = not (Map.member blockId perfs)
    where perfs = Cmd.statePerformanceThreads $ Cmd.statePlay state

-- | Start a new performance thread.
--
-- Pull previous caches from the existing performance, if any.  Use them to
-- generate a new performance, kick off a thread for it, and insert the new
-- thread into 'Cmd.state_performance_threads' and performance into
-- 'Cmd.state_current_performance'.  It will be promoted to
-- 'Cmd.state_performance' when 'evaluate_performance' completes.
generatePerformance :: Ui.State -> Thread.Seconds -> SendStatus -> BlockId
    -> StateM
generatePerformance uiState wait sendStatus blockId = do
    cmdState <- Monad.State.get
    let (perf, logs) = derive uiState cmdState blockId
    mapM_ Log.write logs
    threadId <- liftIO $ Thread.asyncLogged ("perf:" <> prettys blockId) $ do
        let allocs = Ui.config#UiConfig.allocations #$ uiState
            imConfig = Cmd.configIm (Cmd.stateConfig cmdState)
        let lookupInst = either (const Nothing) Just
                . Cmd.stateLookupInstrument uiState cmdState
        evaluatePerformance
            (if imAllocated allocs then Just imConfig else Nothing)
            lookupInst wait sendStatus (Cmd.scorePath cmdState)
            (Cmd.statePlayMultiplier (Cmd.statePlay cmdState)) blockId perf
    Monad.State.modify $ modifyPlayState $ \st -> st
        { Cmd.statePerformanceThreads = Map.insert blockId
            (Cmd.Thread threadId) (Cmd.statePerformanceThreads st)
        , Cmd.stateCurrentPerformance = Map.insert blockId
            perf (Cmd.stateCurrentPerformance st)
        }
    -- If the derivation somehow failed, then the old performance will remain,
    -- and since there is no thread, this will try again the next time around.

{-# SCC derive #-}
derive :: Ui.State -> Cmd.State -> BlockId -> (Cmd.Performance, [Log.Msg])
derive uiState cmdState blockId = (perf, logs)
    where
    perf = case cmdResult of
        Left err -> brokenPerformance $
            "derivation for " <> showt blockId <> " failed: " <> pretty err
        Right (deriveResult, _, _) -> case deriveResult of
            Nothing -> brokenPerformance $
                "derivation for " <> showt blockId <> " aborted"
            Just result -> performance uiState result
    -- The previous cache comes from the fully evaluated performance, since
    -- otherwise there's no point killing the 'evaluate_performance' thread if
    -- the cache makes all derivation serialized.
    prevCache = maybe mempty Cmd.perfDeriveCache $ Map.lookup blockId $
        Cmd.statePerformance $ Cmd.statePlay cmdState
    -- The damage also comes from the current performance, since that's where
    -- the performance is deriving from.
    damage = maybe mempty Cmd.perfDamage $ Map.lookup blockId $
        Cmd.statePerformance $ Cmd.statePlay cmdState
    (_state, _midi, logs, cmdResult) = Cmd.runId uiState cmdState $
        PlayUtil.deriveBlock prevCache damage blockId

evaluatePerformance :: Maybe Config.Config
    -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> Thread.Seconds -> SendStatus -> FilePath -> RealTime -> BlockId
    -> Cmd.Performance -> IO ()
evaluatePerformance imConfig lookupInst wait sendStatus scorePath
        playMultiplier blockId perf = do
    sendStatus blockId Msg.OutOfDate
    Thread.delay wait
    sendStatus blockId Msg.Deriving
    -- I just force the logs here, and wait for a play to actually write them.
    ((), metric) <- Thread.timeAction $ pure $! Msg.forcePerformance perf
    forM_ (checkDummy lookupInst (Msg.perfEvents perf)) $ \(event, msg) ->
        Log.warn $ "note with dummy instrument: " <> Score.shortEvent event
            <> ": " <> msg
    when (Thread.metricWall metric > 1) $
        Log.notice $ "derived " <> showt blockId <> " in "
            <> Thread.showMetric metric
    -- This should match the adjust0 in 'Cmd.Play.start_adjustment'.
    let adjust0 = case Vector.toList (Cmd.perfEvents perf) of
            event : _ -> max (- Score.eventStart event) 0
            _ -> 0
    (procs, events) <- case imConfig of
        Nothing -> pure ([], Cmd.perfEvents perf)
        Just config -> evaluateIm config lookupInst scorePath
            adjust0 playMultiplier blockId (Cmd.perfEvents perf)
    sendStatus blockId $ Msg.DeriveComplete
        (perf { Cmd.perfEvents = events })
        (if null procs then Msg.ImUnnecessary else Msg.ImStarted)
    ok <- case imConfig of
        Just config -> watchSubprocesses blockId
            (makeStatus (Cmd.perfInvTempo perf)
                (stateWantsWaveform (Cmd.perfUiState perf))
                (Config.imDir config) scorePath adjust0 playMultiplier)
            (sendStatus blockId)
            (Set.fromList procs)
        Nothing -> pure True
    unless (null procs) $ do
        stats <- case (ok, imConfig) of
            (True, Just config) -> fmap Just $ imGc $
                Config.outputDirectory (Config.imDir config) scorePath blockId
            _ -> pure Nothing
        sendStatus blockId $ Msg.ImStatus blockId Set.empty $
            Msg.ImComplete (not ok) stats

imGc :: FilePath -> IO ImGc.Stats
imGc outputDir = do
    stats <- ImGc.gc outputDir
    when (ImGc._deletedFiles stats > 0) $
        Log.notice $ ImGc.showStats stats
    pure stats

-- | Return events with a 'Inst.Dummy' backend.  These shouldn't have made it
-- through to actual performance.
checkDummy :: (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> Vector.Vector Score.Event -> [(Score.Event, Text)]
checkDummy lookupInst = snd . Vector.foldl' go (HashSet.empty, [])
    where
    go (seen, warns) event
        | HashSet.member inst seen = (seen, warns)
        | otherwise =
            ( HashSet.insert inst seen
            , maybe warns (\msg -> (event, msg) : warns) (isDummy inst)
            -- , if is_dummy inst then event : warns else warns
            )
        where
        inst = Score.eventInstrument event
    isDummy inst = case Cmd.instBackend <$> lookupInst inst of
        Just (Cmd.Dummy msg) -> Just msg
        _ -> Nothing

stateWantsWaveform :: Ui.State -> TrackId -> Bool
stateWantsWaveform state trackId = maybe False Track.trackWaveform $
    Map.lookup trackId (Ui.stateTracks state)

type Process = (FilePath, [String])

-- | Watch subprocesses and convert their msgs to Msg.DeriveStatus.
watchSubprocesses :: BlockId -> (Config.Message -> ImStatus)
    -> (Msg.DeriveStatus -> IO ()) -> Set Process -> IO Bool
watchSubprocesses rootBlockId makeStatus sendStatus procs =
    streamSubprocesses procs $ S.mapM_ watch
    where
    watch ((cmd, args), ProcessExit code)
        | code /= Processes.ExitCode 0 = Log.warn $ "subprocess failed with "
            <> showt code <> ": " <> Text.unwords (map txt (cmd : args))
        | otherwise = pure ()
    watch (_, Message msg)
        -- This is an update for a child block.  I only display progress
        -- for each block as its own toplevel.  If a block is child of
        -- another, I can see its prorgess in the parent.
        | Config._blockId msg /= rootBlockId = pure ()
        | otherwise = case makeStatus msg of
            ImStatus status -> sendStatus =<< resolveWaveformLinks status
            ImWarn msg -> Log.write msg
            ImFail msg -> Log.write msg
            ImNothing -> pure ()

-- | Like 'watch_subprocesses', but notify the callback as soon as
-- they have all rendered enough audio.
waitForSubprocesses :: IO () -> Set ScoreT.Instrument -> Set Process
    -> IO Bool
waitForSubprocesses ready expectedInstruments procs =
    streamSubprocesses procs $ Streams.mapAccumL_ check (Set.empty, Set.empty)
    where
    check (started, insts) ((cmd, args), ProcessExit code) = do
        unless (code == Processes.ExitCode 0) $
            Log.warn $ "subprocess failed with "
                <> showt code <> ": " <> Text.unwords (map txt (cmd : args))
        pure (started, insts)
    check (started, insts) (proc, Message msg) = case Config._payload msg of
        Config.WaveformsCompleted chunks
            | 0 `elem` chunks -> do
                -- TODO compute chunk for a specific time, not always 0
                let started2 = Set.insert proc started
                    insts2 = Set.insert (Config._instrument msg) insts
                Log.debug $ "started: " <> pretty started2
                    <> ", insts: " <> pretty insts2
                    <> ", expected: " <> pretty expectedInstruments
                when (started2 == procs && insts2 == expectedInstruments) ready
                pure (started2, insts2)
            | otherwise -> ignore
        Config.Failure err -> Log.warn err >> ignore
        Config.Warn stack msg -> Log.warnStack stack msg >> ignore
        Config.RenderingRange {} -> ignore
        where ignore = pure (started, insts)

data Progress = ProcessExit Processes.Exit | Message Config.Message

-- | Watch each subprocess, return when they all exit.  The stream returns
-- False if there was some failure along the way.
--
-- This is a bracket and doesn't return the stream because I want to kill the
-- subprocesses when leaving the scope.
streamSubprocesses :: Set Process
    -> (S.Stream (S.Of (Process, Progress)) IO Bool -> IO a) -> IO a
streamSubprocesses procs action
    | Set.null procs = action $ pure False
    | otherwise = Processes.multipleOutput (Set.toList procs) $ \chan ->
        action $ Control.loop1 (procs, True) $ \loop (procs, ok) -> if
            | Set.null procs -> pure ok
            | otherwise -> do
                (proc, out) <- liftIO $ Chan.readChan chan
                loop =<< process procs ok proc out
    where
    process procs ok proc = \case
        Processes.Stderr line -> put line >> pure (procs, ok)
        Processes.Stdout line -> do
            ok1 <- progress proc line
            pure (procs, ok && ok1)
        Processes.Exit code -> do
            S.yield (proc, ProcessExit code)
            pure (Set.delete proc procs, ok && code == Processes.ExitCode 0)
    progress proc line = case Config.parseMessage line of
        Nothing -> do
            put $ "couldn't parse: " <> line
            pure False
        Just msg -> do
            S.yield (proc, Message msg)
            pure $ case Config._payload msg of
                Config.Failure {} -> False
                _ -> True
    -- These get called concurrently, so avoid jumbled output.
    put line = liftIO $ Log.withStdioLock $ Text.IO.hPutStrLn IO.stdout line

data ImStatus =
    ImStatus Msg.DeriveStatus | ImWarn Log.Msg | ImFail Log.Msg | ImNothing
    deriving (Show)

-- | Convert the status output from an im subprocess to an 'ImStatus'.
makeStatus :: Transport.InverseTempoFunction -> (TrackId -> Bool) -> FilePath
    -> FilePath -> RealTime -> RealTime -> Config.Message -> ImStatus
makeStatus invTempo wantsWaveform imDir scorePath adjust0 playMultiplier
        (Config.Message blockId trackIds instrument payload) =
    case payload of
        Config.RenderingRange start end ->
            status $ Msg.ImRenderingRange instrument start end
        Config.WaveformsCompleted chunknums
            | Set.null wantedTrackIds -> ImNothing
            | otherwise -> case mapM makeWaveform chunknums of
                Right waveforms -> status $ Msg.ImWaveformsCompleted waveforms
                Left err -> ImWarn $ Log.msg Log.Warn Nothing $
                    "to_score for " <> pretty chunknums <> ": " <> err
        Config.Warn stack err -> ImWarn $ Log.msg Log.Warn (Just stack) $
            "im instrument " <> pretty instrument <> ": " <> err
        Config.Failure err -> ImFail $ Log.msg Log.Warn Nothing $
            "im failure: " <> pretty blockId <> ": " <> pretty trackIds
            <> ": " <> err
    where
    status = ImStatus . Msg.ImStatus blockId wantedTrackIds
    wantedTrackIds = Set.filter wantsWaveform trackIds
    makeWaveform chunknum = do
        (start, ratios) <- chunkRatios toScore
            (timeAt chunknum) (timeAt (chunknum+1)) ratiosPerChunk
        adjust0 <- toScore adjust0
        -- Debug.tracepM "start, ratio"
        --     ((time_at chunknum, time_at (chunknum+1)), start, ratios)
        pure $ Track.WaveformChunk
            { _filename = Config.chunkPath imDir scorePath blockId
                instrument chunknum
            , _chunknum = chunknum
            , _start = start * RealTime.toScore playMultiplier - adjust0
            , _ratios = map (/ RealTime.toSeconds playMultiplier) ratios
            }
    timeAt chunknum = RealTime.seconds $
        fromIntegral $ chunknum * Config.chunkSeconds
    -- If this fails, it means I don't have any tempo info for this
    -- (block_id, track_id), which likely means the block or track failed
    -- to derive, at which point I shouldn't have gotten any notes from it.
    -- So this shouldn't happen.
    toScore t = tryJust ("no ScoreTime for " <> pretty t) $
        realToScore invTempo blockId trackIds t

-- | How many ratios to put into 'Track._ratios'.  This will make waveform
-- display more accurate when tempos change, since it's the resolution used
-- to warp the waveform to show in ScoreTime.  'chunk_ratios' calculates the
-- actual ratios.  The cost is that it's more expensive to sample more
-- ratios, and ship them to C++, and is a waste in the common case of a
-- constant tempo.
--
-- TODO It would be better to give exact tempo breakpoints, but since I work
-- with Warp rather than a tempo signal, I'd have to get the derivative to
-- get tempo back, and it will have to be sampled anyway if it's changing.
-- Also, it would be more efficient for the whole block or tracks to get
-- a single tempo signal so it can warp waveform accordingly, rather than
-- every chunk getting its own independent ratios.  At the moment, it dosen't
-- seem like a big priority if waveform display is a bit inaccurate, because
-- I'm not using it to line anything up.
ratiosPerChunk :: Int
ratiosPerChunk = 2

chunkRatios :: (RealTime -> Either Text ScoreTime) -> RealTime -> RealTime
    -> Int -> Either Text (ScoreTime, [Double])
chunkRatios toScore start end n = do
    scoreBps <- mapM toScore bps
    pure (head scoreBps, zipWith ratioAt scoreBps (drop 1 scoreBps))
    where
    ratioAt start end =
        (fromIntegral Config.chunkSeconds / fromIntegral n)
            / ScoreTime.toDouble (end - start)
    bps = Lists.rangeEnd start end ((end - start) / fromIntegral n)

-- | Resolve symlinks in 'Track._filename's.  The PeakCache uses the filename
-- as a key, so I can get false hits if I use the symlinks, which all look like
-- 000.wav.
--
-- I don't do this in 'make_status' because that would put the whole thing into
-- IO.
resolveWaveformLinks :: Msg.DeriveStatus -> IO Msg.DeriveStatus
resolveWaveformLinks (Msg.ImStatus blockId trackIds
        (Msg.ImWaveformsCompleted waveforms)) = do

    -- resolved <- mapM (Directory.getSymbolicLinkTarget . Track._filename)
    --     waveforms
    -- when (any (not . ("checkpoint/" `List.isPrefixOf`)) resolved) $ do
    --     Log.error $ "bad symlink in: " <> pretty
    --         (zip (map Track._filename waveforms) resolved)
    --     let dir = FilePath.takeDirectory $ Track._filename (head waveforms)
    --     Process.callProcess "ls" ["-l", dir]

    fns <- mapM (resolveLink . Track._filename) waveforms
    -- I got a directory in there once and don't know why...
    when (any (not . (".wav" `List.isSuffixOf`)) fns) $ do
        Log.error $ "waveforms resolved to non-wav: "
            <> pretty (zip (map Track._filename waveforms) fns)
        fns2 <- mapM (resolveLink . Track._filename) waveforms
        Log.error $ "tried again:"
            <> pretty (zip (map Track._filename waveforms) fns2)

    pure $ Msg.ImStatus blockId trackIds $ Msg.ImWaveformsCompleted
        [wave { Track._filename = fn } | (fn, wave) <- zip fns waveforms]
resolveWaveformLinks status = pure status

resolveLink :: FilePath -> IO FilePath
resolveLink fname = do
    dest <- Directory.getSymbolicLinkTarget fname
    dest <- if "checkpoint/" `List.isPrefixOf` dest
            && ".wav" `List.isSuffixOf` dest
        then pure dest
        else do
            dest2 <- Directory.getSymbolicLinkTarget fname
            Log.error $ "bad symlink: " <> showt fname <> " -> " <> showt dest
                <> ", next time: " <> showt dest2
            pure dest2
    pure $ FilePath.takeDirectory fname </> dest

-- resolve_link :: FilePath -> IO FilePath
-- resolve_link fname =
--     (FilePath.takeDirectory fname </>) <$> Directory.getSymbolicLinkTarget fname

realToScore :: Transport.InverseTempoFunction -> BlockId -> Set TrackId
    -> RealTime -> Maybe ScoreTime
realToScore invTempo blockId trackIds =
    fmap snd . List.find ((`Set.member` trackIds) . fst) <=< lookup blockId
        . invTempo Transport.NoStop

-- | If there are im events, serialize them and return a Processes to render
-- them, and the non-im events.
evaluateIm :: Config.Config
    -> (ScoreT.Instrument -> Maybe Cmd.ResolvedInstrument)
    -> FilePath -> RealTime -> RealTime -> BlockId -> Vector.Vector Score.Event
    -> IO ([Process], Vector.Vector Score.Event)
evaluateIm config lookupInst scorePath adjust0 playMultiplier blockId
        events = do
    procs <- Maybe.catMaybes <$> mapM writeNotes bySynth
    Config.clearUnusedInstruments outputDir instruments
    pure (procs, fromMaybe mempty $ lookup Nothing bySynth)
    where
    instruments = Vector.foldl' add mempty events
        where add = flip $ HashSet.insert . Score.eventInstrument
    bySynth = Util.Vector.partitionOn imSynth events
    lookupIm inst = case lookupInst inst of
        Just inst
            | Inst.Inst (Inst.Im {}) _  <- Cmd.instInstrument inst ->
                Just inst
        _ -> Nothing
    imSynth = fmap Cmd.instSynth . lookupIm . Score.eventInstrument

    outputDir = Config.outputDirectory (Config.imDir config) scorePath
        blockId
    writeNotes (Just synthName, events) =
        case Map.lookup synthName (Config.synths config) of
            Just synth -> do
                let notesFile = Config.notesFilename (Config.imDir config)
                        scorePath blockId synth
                -- I used to get the changed flag out of Im.Convert.write and
                -- skip the subprocess if it hadn't changed.  But that gets in
                -- the way of getting waveforms on the first run (assuming the
                -- notes haven't been touched since).  In any case, I'm now
                -- more careful in insert_damage, so we shouldn't even get here
                -- unless there was damage on the block, and if there was but
                -- notes haven't changed, the im synth should hit its cache.
                Im.Convert.write adjust0 playMultiplier blockId
                    lookupInst notesFile events
                let binary = Config.binary synth
                pure $ if null binary
                    then Nothing
                    else Just (binary, ["--progress", notesFile, outputDir])
            Nothing -> do
                Log.warn $ "unknown im synth " <> showt synthName <> " with "
                    <> showt (Vector.length events) <> " events"
                pure Nothing
    writeNotes (Nothing, _) = pure Nothing

-- | If there are no UiConfig.Im instruments, then I don't need to bother to
-- partition out its events.  However, it means I won't get errors if there
-- happen to be any, but I'll worry about that if it becomes a problem.
imAllocated :: UiConfig.Allocations -> Bool
imAllocated (UiConfig.Allocations allocs) =
    any ((==UiConfig.Im) . UiConfig.allocBackend) (Map.elems allocs)

-- | Make a broken performance with just an error msg.  This ensures that
-- the msg is logged when you try to play, but will still suppress further
-- performance, so you don't get a million msgs.
brokenPerformance :: Text -> Cmd.Performance
brokenPerformance msg = Cmd.Performance
    { perfDeriveCache = mempty
    , perfEvents = mempty
    , perfLogs = [Log.msg Log.Warn Nothing msg]
    , perfLogsWritten = False
    , perfTrackDynamic = mempty
    , perfIntegrated = mempty
    , perfDamage = mempty
    , perfWarps = mempty
    , perfTrackSignals = mempty
    , perfBlockDeps = mempty
    , perfTrackInstruments = mempty
    , perfUiState = Ui.empty
    }

-- | Constructor for 'Cmd.Performance'.
performance :: Ui.State -> Derive.Result -> Cmd.Performance
performance state result = Cmd.Performance
    { perfDeriveCache = Derive.rCache result
    , perfEvents = vevents
    , perfLogs = logs
    , perfLogsWritten = False
    , perfTrackDynamic = Derive.rTrackDynamic result
    , perfIntegrated = Derive.rIntegrated result
    , perfDamage = mempty
    , perfWarps = Derive.rTrackWarps result
    , perfTrackSignals = Derive.rTrackSignals result
    , perfBlockDeps = Derive.collectBlockDeps $ Derive.stateCollect $
        Derive.rState result
    , perfTrackInstruments = trackInstruments
        vevents (Map.keys (Ui.stateTracks state))
    , perfUiState = state
    }
    where
    vevents = Vector.fromList events
    (events, logs) = Stream.partition (Derive.rEvents result)

modifyPlayState :: (Cmd.PlayState -> Cmd.PlayState) -> Cmd.State -> Cmd.State
modifyPlayState modify state =
    state { Cmd.statePlay = modify (Cmd.statePlay state) }

trackInstruments :: Vector.Vector Score.Event -> [TrackId]
    -> Map TrackId (Set ScoreT.Instrument)
trackInstruments events = Map.Lazy.fromAscList . Lists.keyOnSnd instrumentsOf
    -- fromAscList should be safe since Map.keys returns in order.
    where
    -- Go by TrackId, so I can make each Map value lazy, since I will only
    -- even need the instruments if I do an im mute on the track.
    instrumentsOf trackId = Vector.foldl' (get trackId) mempty events
    get trackId seen event
        | trackId `elem` trackIds =
            Set.insert (Score.eventInstrument event) seen
        | otherwise = seen
        where trackIds = Stack.trackIdsOf (Score.eventStack event)
