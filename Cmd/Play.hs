-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Master control for playing blocks.

    Here's how it works:

    - Find the relevant block to play.

    - Deriver generates a performable score and an inverse tempo map.

    - The score is preprocessed by adding the current absolute time to it
    and skipping notes based on the start offset.

    - Create a 'Transport.PlayControl' to tell players when to stop, and a
    'Transport.ActivePlayers' to track the number of players still running.
    Start players for each kind of backend: 'Perform.Midi.Play.player_thread'
    for MIDI and im-via-MIDI, one for SC if appropriate, and an audio streaming
    thread if play_im_direct is enabled.

    - The ActivePlayers and tempo map are passed to
    'Cmd.PlayC.play_monitor_thread', which uses the tempo map to display the
    play position in the various blocks.  It stops when it runs out of tempo
    map (which corresponds with running off the end of the score), or when
    the PlayControl goes to Stop.  It's not synchronized to the play threads in
    any way, but of course they are both working from the same score.

    - A stop from the user sets 'Transport.stop_player'.  All the players
    and the play_monitor_thread are polling it and will quit.

    - Another thread is watching the ActivePlayers.  It sent Transport.Playing
    to the responder when they started, and will send Transport.Stopped when
    they go to zero.  This control's the UI's idea of whether it's playing or
    not.  If it's playing, it still has the PlayControl, so it won't accept
    another play, but will accept a stop.  So player threads should only exit
    when there's nothing left for them to cancel, so MIDI and OSC, being
    scheduled in advance, will hang around even after they scheduled their last
    message.  If a player never sends Transport.player_stopped, ActivePlayers
    will never go to 0, and we get stuck.  So they should do the stop in a
    finally block.

    The im backend complicates things a bit.  See NOTE [play-im].

    repeat_at, the play speed multiplier, and the negative start adjustment
    also complicate things.
-}
module Cmd.Play where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Audio.AudioT as AudioT
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Vector

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Cache as Cache
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Midi.Midi as Midi
import qualified Perform.Im.Play as Im.Play
import qualified Perform.Midi.Patch as Patch
import qualified Perform.RealTime as RealTime
import qualified Perform.Sc.Note as Sc.Note
import qualified Perform.Sc.Play as Sc.Play
import qualified Perform.Transport as Transport

import qualified Synth.Shared.Config as Shared.Config
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Zoom as Zoom

import           Global
import           Types


modifyPlayMultiplier :: Cmd.M m => (RealTime -> RealTime) -> m ()
modifyPlayMultiplier f = do
    Cmd.modifyPlayState $ \st -> st
        { Cmd.statePlayMultiplier = to1 $ f (Cmd.statePlayMultiplier st) }
    -- A change in state_play_multiplier is not damage for MIDI because it only
    -- affects playback, but it is damage for im, which needs to rerender
    -- audio.  Since there is no change to Ui.State, I have to force a rederive
    -- here.
    whenM hasIm Cmd.invalidatePerformances
    where
    -- Set to 1 if I'm this close.  Otherwise repeated multiplies don't
    -- necessarily come back exactly.
    to1 val
        | abs (1 - val) <= threshold = 1
        | otherwise = val
    threshold = 0.01

-- * stop

-- | Context sensitive stop that stops whatever is going on.  First it stops
-- realtime play, then step play, and then it just sends all notes off.  If
-- it does the last one, it returns False in case you want to go stop something
-- else.
cmdContextStop :: Cmd.CmdT IO Bool
cmdContextStop = gets Cmd.statePlayControl >>= \case
    [] -> do
        stepPlaying <- Cmd.gets $
            Maybe.isJust . Cmd.stateStep . Cmd.statePlay
        if stepPlaying
            then StepPlay.cmdClear >> pure True
            -- play_cache may still be streaming after the karya transport
            -- stops.
            else do
                Cmd.allNotesOff
                stopIm
                liftIO Sc.Play.forceStop
                pure False
    ctls -> stop ctls >> pure True

cmdStop :: Cmd.CmdT IO Cmd.Status
cmdStop = do
    stop =<< gets Cmd.statePlayControl
    pure Cmd.Done

stop :: [Transport.PlayControl] -> Cmd.CmdT IO ()
stop ctls = do
    liftIO $ mapM_ Transport.stopPlayer ctls
    stopIm

-- | Stop im stream, if playing.  See NOTE [play-im].
stopIm :: Cmd.CmdT IO ()
stopIm = whenJustM imAddr $ \(wdev, chan) ->
    Cmd.midi wdev $ Midi.ChannelMessage chan Im.Play.stop

hasIm :: Cmd.M m => m Bool
hasIm = Maybe.isJust <$> imAddr

-- * play

setPreviousPlay :: Cmd.M m => Text -> Cmd.CmdId Cmd.PlayArgs -> m ()
setPreviousPlay name cmd = Cmd.modifyPlayState $ \st ->
    st { Cmd.statePreviousPlay = Just (Cmd.PlayCmd name cmd) }

-- | Play the local block from its beginning.
localBlock :: Cmd.M m => m Cmd.PlayArgs
localBlock = do
    blockId <- Cmd.getFocusedBlock
    fromScore blockId Nothing 0 Nothing

-- | Start playing from the point selection on the local block.  If the
-- selection is a range, loop that range forever.
localSelection :: Cmd.M m => m Cmd.PlayArgs
localSelection = do
    (blockId, _, trackId, _) <- Selection.getInsert
    sel <- Selection.get
    let (pos, repeatAt) = if Sel.isPoint sel
            then (Sel.startPos sel, Nothing)
            else Just <$> Sel.range sel
    fromScore blockId (Just trackId) pos repeatAt

-- | Play the current block's performance from the previous
-- 'Cmd.state_play_step'.
localPrevious :: Cmd.M m => m Cmd.PlayArgs
localPrevious = do
    step <- gets Cmd.statePlayStep
    (blockId, tracknum, trackId, pos) <- Selection.getInsert
    prev <- TimeStep.rewind step blockId tracknum pos
    localFrom blockId trackId (fromMaybe 0 prev)

-- | Play the current block's performance from the top of the window.
localTop :: Cmd.M m => m Cmd.PlayArgs
localTop = do
    (blockId, trackId, top) <- topOfBlock
    localFrom blockId trackId top

localFrom :: Cmd.M m => BlockId -> TrackId -> TrackTime -> m Cmd.PlayArgs
localFrom blockId trackId pos =
    fromScore blockId (Just trackId) pos Nothing

-- | Play the root block from its beginning.
rootBlock :: Cmd.M m => m Cmd.PlayArgs
rootBlock = do
    Ui.lookupRootId >>= \case
        Nothing -> localBlock
        Just rootId -> fromScore rootId Nothing 0 Nothing

-- | Play the root performance from the selection on the root block.  This
-- is useful to manually set a point to start playing.
rootFromRootSelection :: Cmd.M m => m Cmd.PlayArgs
rootFromRootSelection = do
    (blockId, _, trackId, pos) <- Selection.getRootInsert
    fromScore blockId (Just trackId) pos Nothing

-- | The same as 'local_selection', but use the root performance.
rootSelection :: Cmd.M m => m Cmd.PlayArgs
rootSelection = do
    (blockId, _, trackId, _) <- Selection.getInsert
    sel <- Selection.get
    let (pos, repeatAt)
            | Sel.isPoint sel = (Sel.startPos sel, Nothing)
            | otherwise = Just <$> Sel.range sel
    Ui.lookupRootId >>= \case
        Nothing -> localSelection
        Just rootId -> do
            perf <- getPerformance rootId
            let realtimeAt = Perf.lookupRealtime perf blockId (Just trackId)
            realRepeatAt <- maybe (pure Nothing) realtimeAt repeatAt
            maybe localSelection (fromRealtime rootId realRepeatAt)
                =<< realtimeAt pos

-- | Find the previous step on the focused block, get its RealTime, and play
-- from the root at that RealTime.  If this block isn't linked from the root,
-- then fall back on 'local_previous'.
rootPrevious :: Cmd.M m => m Cmd.PlayArgs
rootPrevious = do
    (blockId, tracknum, trackId, pos) <- Selection.getInsert
    step <- gets Cmd.statePlayStep
    prev <- fromMaybe pos <$> TimeStep.rewind step blockId tracknum pos
    rootFrom blockId trackId prev

-- | Like 'root_previous', but play from the top of the selected block.
rootTop :: Cmd.M m => m Cmd.PlayArgs
rootTop = do
    (blockId, trackId, top) <- topOfBlock
    rootFrom blockId trackId top

topOfBlock :: Cmd.M m => m (BlockId, TrackId, TrackTime)
topOfBlock = do
    (blockId, _, trackId, _) <- Selection.getInsert
    viewId <- Cmd.getFocusedView
    top <- Zoom.offset . Block.viewZoom <$> Ui.getView viewId
    pure (blockId, trackId, top)

rootFrom :: Cmd.M m => BlockId -> TrackId -> TrackTime -> m Cmd.PlayArgs
rootFrom blockId trackId pos = do
    playRoot <- maybeRootFrom blockId trackId pos
    maybe (localFrom blockId trackId pos) pure playRoot

maybeRootFrom :: Cmd.M m => BlockId -> TrackId -> ScoreTime
    -> m (Maybe Cmd.PlayArgs)
maybeRootFrom blockId trackId pos =
    justm Ui.lookupRootId $ \rootId -> do
        perf <- getPerformance rootId
        justm (Perf.lookupRealtime perf blockId (Just trackId) pos) $
            \start -> Just <$> fromRealtime rootId Nothing start

fromScore :: Cmd.M m => BlockId
    -> Maybe TrackId -- ^ Track to play from.  Since different tracks can have
    -- different tempos, a track is needed to convert to RealTime.  If not
    -- given, use the first track that has tempo information.
    -> ScoreTime -- ^ Convert to RealTime and start playing from this time.
    -> Maybe ScoreTime
    -> m Cmd.PlayArgs
fromScore blockId startTrack startPos repeatAt = do
    start <- getRealtime blockId blockId startTrack startPos
    repeatAt <- maybe (pure Nothing)
        (fmap Just . getRealtime blockId blockId startTrack) repeatAt
    fromRealtime blockId repeatAt start

getRealtime :: Cmd.M m => BlockId
    -- ^ Lookup realtime according to the performance of this block.
    -> BlockId
    -- ^ Lookup realtime at the position (TrackId, ScoreTime) within this block.
    -> Maybe TrackId -> ScoreTime
    -> m RealTime
getRealtime perfBlock playBlock maybeTrackId pos = do
    perf <- getPerformance perfBlock
    maybeStart <- Perf.lookupRealtime perf playBlock maybeTrackId pos
    case maybeStart of
        Nothing -> Cmd.throw $ "play " <> showt perfBlock
            <> " has no tempo information"
        Just start -> pure start

getPerformance :: Cmd.M m => BlockId -> m Cmd.Performance
getPerformance blockId = do
    perf <- Perf.getCurrent blockId
    writeLogs blockId perf
    pure perf

writeLogs :: Cmd.M m => BlockId -> Cmd.Performance -> m ()
writeLogs blockId perf = unless (Cmd.perfLogsWritten perf) $ do
    -- There are so many cache msgs it clogs up logview.  I'm writing a summary
    -- anyway so I can filter them out.
    mapM_ Log.write $ filter (not . Cache.isCacheLog) (Cmd.perfLogs perf)
    -- Logview can only display one set of stats, so only show the root block.
    whenM ((== Just blockId) <$> Ui.lookupRootId) $
        recordCacheStats (Cmd.perfLogs perf)
    Cmd.modifyPlayState $ \st -> st
        { Cmd.stateCurrentPerformance = Map.insert blockId
            (perf { Cmd.perfLogsWritten = True })
            (Cmd.stateCurrentPerformance st)
        }

-- | Summarize the cache stats and emit them as global status msgs.
--
-- The output looks like
--
-- > ~C: [34 / 6742] bid bid bid... || ~X control damage: [104] bid bid ... ||
-- > ~X trock block damage: [1] bid
--
-- This means that 34 blocks were cached, totally 6742 events.  104 blocks
-- were not cached due to control damage, and 1 more due to track block damage.
-- The reasons are from 'Derive.Cache.find_generator_cache'.  They keys are
-- prefixed with a tilde to make them sort last in the logview status line.
--
-- 'Cmd.Repl.LPerf.cache_stats' gives a more complete summary.
recordCacheStats :: Cmd.M m => [Log.Msg] -> m ()
recordCacheStats logs = do
    let (rederived, cached) = extractCacheStats getBlockId logs
    Cmd.setGlobalStatus "~C" $ "[" <> showt (length cached) <> " / "
        <> showt (Num.sum (map snd cached)) <> "] "
        <> elide (Text.unwords (map (Id.identName . fst) cached))
    statusKeys <- Cmd.gets (Map.keysSet . Cmd.stateGlobalStatus)
    let keys = map (("~X "<>) . fst) rederived
        gone = Set.filter ("~X " `Text.isPrefixOf`) $
            statusKeys Set.\\ Set.fromList keys
    forM_ (zip keys (map snd rederived)) $ \(key, blockIds) ->
        Cmd.setGlobalStatus key $ "[" <> showt (length blockIds) <> "] "
            <> elide (Text.unwords (map Id.identName blockIds))
    forM_ (Set.toList gone) $ \key -> Cmd.setGlobalStatus key ""
    where
    maxChars = 45
    elide s
        | Text.length s > maxChars = Text.take (maxChars-3) s <> "..."
        | otherwise = s

extractCacheStats :: (Log.Msg -> Maybe k) -> [Log.Msg]
    -> ([(Text, [k])], [(k, Int)])
    -- ^ (cache misses, cache hits):
    -- ([(because, [key])], [(key, cached_vals)])
extractCacheStats key logs = (rederived, cached)
    where
    -- [("because xyz", [bid, bid, bid, ...])]
    rederived = map (second (map fst)) $ Lists.keyedGroupSort snd
        [(blockId, because) | (blockId, Left because) <- stats]
    -- [(bid1, 42), (bid2, 32), ...]
    cached = [(blockId, vals) | (blockId, Right vals) <- stats]
    stats = mapMaybe extract logs
    extract log = case key log of
        Nothing -> Nothing
        Just blockId
            | Just because <- Cache.cacheMissReason log ->
                Just (blockId, Left because)
            | Just vals <- Cache.cacheHitEvents log ->
                Just (blockId, Right vals)
            | otherwise -> Nothing

-- | Get block cache stats.
getBlockId :: Log.Msg -> Maybe BlockId
getBlockId = Stack.blockOf <=< Lists.head . Stack.innermost <=< Log.msgStack

-- | Get track cache stats.
getTrackId :: Log.Msg -> Maybe (BlockId, TrackId)
getTrackId = Stack.blockTrackOf <=< Log.msgStack

-- | Play the performance of the given block starting from the given time.
fromRealtime :: Cmd.M m => BlockId -> Maybe RealTime -> RealTime
    -> m Cmd.PlayArgs
fromRealtime blockId repeatAt start_ = do
    -- Since 0 is considered "the beginning", even if that happens to be before
    -- 0, there's no point asking for something before 0, and will just cause
    -- play to seem to wedge for a moment.  'PlayUtil.perform_from' has
    -- a special hack to notice and include notes < 0.
    let start = max 0 start_
    perf <- Perf.getCurrent blockId
    multiplier <- gets (recip . Cmd.statePlayMultiplier)

    maybeSync <- gets Cmd.stateSync
    case maybeSync of
        -- Don't bother with a MMC Goto if I'm going to send MTC.
        Just sync | not (Cmd.syncMtc sync) ->
            Cmd.midi (Cmd.syncDevice sync) $ Selection.mmcGoto sync start
        _ -> pure ()
    -- MTC rounds up to the previous whole frame, so the mtc might start
    -- slightly before the notes.
    -- TODO actually DAWs need a bit of time to sync, so maybe I should start
    -- further in advance.
    let mtc = PlayUtil.shiftMidi 1 start $ map LEvent.Event $
            generateMtc maybeSync start

    imPlayDirect <- Cmd.gets $ Cmd.configImPlayDirect . Cmd.stateConfig
    imInsts <- getImInsts
    mbPlayCacheAddr <- if imPlayDirect || Set.null imInsts
        then pure Nothing
        else imAddr
    muted <- Perf.mutedImInstruments blockId
    scorePath <- Cmd.gets Cmd.scorePath
    let imMsgs = maybe []
            (imPlayMsgs scorePath blockId muted (start * multiplier))
            mbPlayCacheAddr

    (midiMsgs, scMsgs) <- PlayUtil.performFrom start (Cmd.perfEvents perf)
    let adjust0 = getAdjust0 start (not (null imMsgs)) midiMsgs
            (Cmd.perfEvents perf)
    midiMsgs <- pure $ PlayUtil.shiftMidi multiplier adjust0 midiMsgs

    -- See doc for "Cmd.PlayC" for why I return a magic value.
    pure $ Cmd.PlayArgs
        { playSync = maybeSync
        , playName = pretty blockId
        , playMidi = imMsgs ++ mergeMidi midiMsgs mtc
        , playSc = Sc.Note.PlayNotes
            { shift = adjust0, stretch = multiplier, notes = scMsgs }
        , playInvTempo = Just $ \stop ->
            Cmd.perfInvTempo perf stop . (+adjust0) . (/multiplier)
        , playRepeatAt = (*multiplier) . subtract start <$> repeatAt
        , playImEnd = if Set.null imInsts then Nothing
            else subtract adjust0 . Score.eventEnd <$> Util.Vector.findEnd
                ((`Set.member` imInsts) . Score.eventInstrument)
                (Cmd.perfEvents perf)
        , playImDirect = if Set.null imInsts || not imPlayDirect
            then Nothing
            else Just $ Cmd.PlayDirectArgs
                { playScorePath = scorePath
                , playBlockId = blockId
                , playMuted = muted
                , playStart = start * multiplier
                }
        }

getAdjust0 :: RealTime -> Bool -> [LEvent.LEvent Midi.WriteMessage]
    -> Vector.Vector Score.Event -> RealTime
getAdjust0 start hasIm midiMsgs events = negativeStart - imLatency
    where
    -- Events can wind up before 0, say if there's a grace note on a note at 0.
    -- To have them play correctly, perform_from will give me negative events
    -- when starting from 0, and then I have to shift the start time back to
    -- consider the first event the new 0.
    negativeStart
        | start == 0 && fstMsg < 0 = fstMsg
        | otherwise = start
    -- The first MIDI msg will incorporate any possible leading controls.
    -- The first Score.Event should be after that, unless there's an im
    -- event at negative time.
    fstMsg = min firstScore (PlayUtil.firstTime midiMsgs)
    firstScore = maybe 0 Score.eventStart $ Lists.head $ Vector.toList $
        Vector.take 1 events
    imLatency = if hasIm
        then toSeconds Shared.Config.startLatency else 0
    -- This duplicates AUtil.toSeconds, but AUtil winds up importing
    -- Audio.Audio, and hence VectorC, which incurs a dep on vector.cc.o.
    toSeconds = RealTime.seconds
        . AudioT.framesToSeconds Shared.Config.samplingRate

getImInsts :: Cmd.M m => m (Set ScoreT.Instrument)
getImInsts = do
    allocs <- Ui.config#UiConfig.allocationsMap <#> Ui.get
    pure $ Set.fromList $ map fst $
        filter (UiConfig.isImAllocation . snd) $ Map.toList allocs

-- TODO: I still support explicitly allocated play-cache for historical
-- reasons, but eventually I should just standardize on config_play_cache?
imAddr :: Cmd.M m => m (Maybe Patch.Addr)
imAddr = do
    allocs <- Ui.config#UiConfig.allocationsMap <#> Ui.get
    configPlayCache <- Cmd.gets $ Cmd.configPlayCache . Cmd.stateConfig
    case List.find isPlayCache (Map.elems allocs) of
        Nothing -> pure configPlayCache
        Just alloc -> case UiConfig.allocBackend alloc of
            UiConfig.Midi config -> case Patch.configAddrs config of
                [addr] -> pure (Just addr)
                [] -> Cmd.throw $
                    pretty UiConfig.playCache <> " allocation with no addrs"
                _ -> Cmd.throw $
                    pretty UiConfig.playCache <> " allocation with >1 addrs"
            _ -> Cmd.throw $
                pretty UiConfig.playCache <> " with non-MIDI allocation"
    where
    isPlayCache = (==UiConfig.playCache) . UiConfig.allocQualified

imPlayMsgs :: FilePath -> BlockId -> Set ScoreT.Instrument -> RealTime
    -> Patch.Addr -> [LEvent.LEvent Midi.WriteMessage]
imPlayMsgs scorePath blockId muted start (wdev, chan) =
    zipWith msg ts $ concat
        [ Im.Play.encodeTime start
        , Im.Play.encodePlayConfig scorePath blockId muted
        , [Im.Play.start]
        ]
    where
    msg t = LEvent.Event . Midi.WriteMessage wdev t . Midi.ChannelMessage chan
    -- 'encode_time' includes the bit position so it doesn't depend on order,
    -- but encode_play_config has to transmit text, so it does depend on order.
    -- With CoreMIDI it seems msgs stay in order even when they have the same
    -- timestamp, I'll put on a timestamp just in case.  They're all in the
    -- past, so they should still be "as fast as possible", and not 10ms, or
    -- whatever it winds up being.
    ts = map RealTime.milliseconds [0..]

-- | Merge a finite list of notes with an infinite list of MTC.
mergeMidi :: [LEvent.LEvent Midi.WriteMessage]
    -> [LEvent.LEvent Midi.WriteMessage] -> [LEvent.LEvent Midi.WriteMessage]
mergeMidi = mergeUntil (LEvent.either Midi.wmsgTs (const 0))

-- | Merge until the leftmost list runs out.
mergeUntil :: Ord k => (a -> k) -> [a] -> [a] -> [a]
mergeUntil key = go
    where
    go xs [] = xs
    go [] _ = []
    go (x:xs) (y:ys)
        | key x <= key y = x : go xs (y:ys)
        | otherwise = y : go (x:xs) ys

generateMtc :: Maybe Cmd.SyncConfig -> RealTime -> [Midi.WriteMessage]
generateMtc (Just sync) start | Cmd.syncMtc sync =
    map make $ (0, Midi.mtcSync rate smpte) : Midi.generateMtc rate frame
    where
    smpte = Midi.frameToSmpte rate frame
    frame = Midi.secondsToFrame rate (RealTime.toSeconds start)
    rate = Cmd.syncFrameRate sync
    make (secs, msg) =
        Midi.WriteMessage (Cmd.syncDevice sync) (RealTime.seconds secs) msg
generateMtc _ _ = []

lookupCurrentPerformance :: Cmd.M m => BlockId -> m (Maybe Cmd.Performance)
lookupCurrentPerformance blockId =
    Map.lookup blockId <$> gets Cmd.stateCurrentPerformance

-- * implementation

gets :: Cmd.M m => (Cmd.PlayState -> a) -> m a
gets f = Cmd.gets (f . Cmd.statePlay)


{- NOTE [play-im]

    Im is the general name for the offline synthesizer framework.  The overall
    setup is that when the sequencer wants to play notes with an im allocation,
    it serializes them to a file and invokes the external synthesizer.  Said
    synthesizer then renders sound to the audio cache, which is a directory of
    sound files.  Those sound files will be played by the PlayCache VST, which
    just streams them into whatever VST host is in use.

    When it's time to play, and 'lookup_im_config' notices im events, it adds
    a specially formatted MIDI msgs to tell PlayCache where to start playing.
    Since the play_monitor_thread can't get any signals from the vst, it
    assumes PlayCache is playing until time passes the last im event, or
    there is a stop request via 'Transport.stop_player'.  TODO Come to think of
    it, this would probably work for MIDI events too, so maybe I could simplify
    things by getting rid of all the monitor and player communication.

    The sequencer sends AllNotesOff when the user requests a stop, which means
    PlayCache should stop playing right away.  Otherwise, it keeps playing
    until the end of the sample, because who knows how much decay that last
    note may have.  The sequencer also sends ResetAllControllers on a stop when
    already stopped, so I can use that too as a signal to stop even if there is
    decay.
-}
