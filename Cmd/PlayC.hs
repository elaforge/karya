-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | The FFI-using part of "Cmd.Play".  It uses the FFI because the play
    monitor wants to directly call to the UI to update the playback indicator,
    which is much more efficient than going through the whole "Ui.Diff" thing.

    But that means "Cmd.Play" would import FFI-using modules, which causes
    a problem for ghci, and, since I want the Cmd.Play functions to be
    available there, for the REPL as well.  So I engage in a hack: the play cmd
    returns a special 'Cmd.Play' value which contains the arguments for the
    play monitor thread.  The responder treats it as a Done but will call
    'play' with the given args.

    Actually since I have *Stub modules now I think this hack is no longer
    needed.  But I'll leave it in place for now since it doesn't seem to
    be hurting anything and it's nice to divide play into low and high level.
-}
{-# LANGUAGE CPP #-}
module Cmd.PlayC (cmdPlayMsg, play) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf

import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Sc.Note as Sc.Note
import qualified Perform.Sc.Play as Sc.Play
import qualified Perform.Transport as Transport

import qualified Synth.ImGc as ImGc
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Fltk as Fltk
import qualified Ui.Sync as Sync
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types

-- This is just so I don't incur a Util.Audio dependency when I don't have im.
-- If I just merge all the im hackage deps into the basic deps then I can lose
-- this.
#include "hsconfig.h"
#ifdef ENABLE_IM
import qualified Synth.StreamAudio as StreamAudio
#endif


-- * cmd_play_msg

-- | Respond to msgs about derivation and playing status.
cmdPlayMsg :: Fltk.Channel -> Msg.Msg -> Cmd.CmdT IO Cmd.Status
cmdPlayMsg uiChan msg = do
    case msg of
        Msg.Transport status -> transportMsg status
        Msg.DeriveStatus blockId status -> deriveStatusMsg blockId status
        _ -> Cmd.abort
    pure Cmd.Done
    where
    transportMsg status = case status of
        Transport.Playing -> setAllPlayBoxes Config.playColor
        -- Either the performer has declared itself stopped, or the play
        -- monitor has declared it stopped.  In any case, I don't need
        -- a transport to tell it what to do anymore.  Note that I just forget
        -- about the the play_ctl, I don't mark it stopped.  This is because
        -- Transport.Stopped comes out when I run out of score to play, but
        -- I may well still be playing the decay of the last note, so I don't
        -- want to send a stop to cut it off.  For MIDI that probably has
        -- no effect anyway, but it would cut off play_cache or play_im_direct.
        Transport.Stopped playCtl -> do
            Cmd.modifyPlayState $ \st -> st
                { Cmd.statePlayControl =
                    List.delete playCtl (Cmd.statePlayControl st)
                }
            -- This will cover up derive status info, but that should be ok.
            -- And play normally only comes after derive is completed.
            setAllPlayBoxes Config.boxColor
    deriveStatusMsg blockId status = do
        whenJust (deriveStatusColor status) (Ui.setPlayBox blockId)
        -- When I get word that a performance is complete, I promote it from
        -- state_current_performance to state_performance.  Previously I used
        -- the performance in the DeriveComplete, but Play may have flipped
        -- the 'Cmd.perf_logs_written' bit.  More subtle, I don't want to
        -- have different versions of the same Performance kicking around
        -- because it has lazy fields, and a less-forced version could keep
        -- data alive.
        case status of
            Msg.DeriveComplete {} -> do
                current <- Cmd.gets $
                    Cmd.stateCurrentPerformance . Cmd.statePlay
                whenJust (Map.lookup blockId current) $ \perf -> do
                    Cmd.modifyPlayState $ \st -> st
                        { Cmd.statePerformance = Map.insert blockId
                            perf (Cmd.statePerformance st)
                        }
                    liftIO $ updateTrackSignals uiChan blockId
                        (Cmd.perfUiState perf) (Cmd.perfTrackSignals perf)
                    updateHighlights uiChan blockId (Cmd.perfEvents perf)
            _ -> pure ()
        handleImStatus uiChan blockId status
    deriveStatusColor status = case status of
        Msg.OutOfDate {} -> Just $ Color.brightness 1.5 Config.busyColor
        Msg.Deriving {} -> Just Config.busyColor
        Msg.DeriveComplete _ Msg.ImStarted ->
            Just $ Color.brightness 0.5 Config.busyColor
        Msg.DeriveComplete _ Msg.ImUnnecessary -> Just Config.boxColor
        Msg.ImStatus _ _ (Msg.ImComplete {}) -> Just Config.boxColor
        Msg.ImStatus {} -> Nothing

setAllPlayBoxes :: Ui.M m => Color.Color -> m ()
setAllPlayBoxes color =
    mapM_ (flip Ui.setPlayBox color) =<< Ui.allBlockIds

type Range = (TrackTime, TrackTime)

-- ** handle im status

handleImStatus :: Fltk.Channel -> BlockId -> Msg.DeriveStatus
    -> Cmd.CmdT IO ()
handleImStatus uiChan rootBlockId = \case
    Msg.DeriveComplete _ Msg.ImStarted ->
        startImProgress uiChan rootBlockId
    Msg.DeriveComplete _ Msg.ImUnnecessary -> pure ()
    Msg.ImStatus blockId trackIds status -> case status of
        Msg.ImWaveformsCompleted waveforms ->
            imWaveformsCompleted uiChan blockId trackIds waveforms
        Msg.ImRenderingRange instrument start end ->
            imRenderingRange uiChan blockId trackIds instrument
                (start, end)
        Msg.ImComplete failed mbStats -> do
            -- If it failed, leave the the progress highlight in place, to
            -- indicate where it crashed.
            unless failed $
                clearImProgress uiChan rootBlockId
            -- I can only GC if the last im thread completed.  This is because
            -- cache entries don't have BlockId, so I don't want to clear the
            -- entries of another block while it's still running.  It seems a
            -- bit sketchy though, since the evaluation thread could get
            -- pre-empted arbitrarily long before exiting.
            running <- Cmd.runningThreads
            when (null running) $
                liftIO $ Sync.gcWaveforms uiChan
            -- Only show the GC stats when rendering the root block.
            whenJust mbStats $ \stats ->
                whenJustM Ui.lookupRootId $ \scoreRoot ->
                    when (scoreRoot == rootBlockId) $
                        -- TODO put this at the end?  Or just delete it.
                        Cmd.setGlobalStatus "im cache" $
                            Pretty.bytes 0 (ImGc._remaining stats)
    _ -> pure ()

startImProgress :: Fltk.Channel -> BlockId -> Cmd.CmdT IO ()
startImProgress uiChan blockId = do
    trackIds <- getImInstrumentTracks blockId
    let pending = ((0, endOfTrack), Config.imPendingColor)
    sels <- resolveTracks
        [((blockId, trackId), pending) | trackId <- trackIds]
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    liftIO $ do
        Sync.clearWaveforms uiChan viewIds
        Sync.setImProgress uiChan sels

getImInstrumentTracks :: Cmd.M m => BlockId -> m [TrackId]
getImInstrumentTracks blockId = do
    trackIds <- Ui.trackIdsOf blockId
    -- TODO This is inaccurate since it doesn't understand dummy instruments.
    -- But good enough for now?  I avoid perf_track_instruments to avoid
    -- crashing before the first performance exists.
    insts <- mapM Perf.inferInstrument trackIds
    allocs <- Ui.config#UiConfig.allocationsMap <#> Ui.get
    let isIm inst = maybe False UiConfig.isImAllocation $
            Map.lookup inst allocs
    pure $ map fst $ filter (maybe False isIm . snd) $ zip trackIds insts

imWaveformsCompleted :: Fltk.Channel -> BlockId -> Set TrackId
    -> [Track.WaveformChunk] -> Cmd.CmdT IO ()
imWaveformsCompleted uiChan blockId trackIds waveforms = do
    byView <- resolveTracks
        [ ((blockId, trackId), waveforms)
        | trackId <- Set.toList trackIds
        ]
    liftIO $ Sync.setWaveforms uiChan byView

-- *** progress

clearImProgress :: Fltk.Channel -> BlockId -> Cmd.CmdT IO ()
clearImProgress uiChan blockId = do
    Cmd.modifyPlayState $ \st -> st
        { Cmd.stateImProgress = Map.delete blockId (Cmd.stateImProgress st)
        }
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    liftIO $ Sync.clearImProgress uiChan viewIds

imRenderingRange :: Fltk.Channel -> BlockId -> Set TrackId
    -> ScoreT.Instrument -> (RealTime, RealTime) -> Cmd.CmdT IO ()
imRenderingRange uiChan blockId trackIds instrument range = do
    ranges <- updateRenderingRanges blockId (Set.toList trackIds)
        instrument range
    -- I could be fancy by setting the color shade based on how many
    -- instruments are rendering in each range, but let's leave it be for now.
    sels <- resolveTracks =<< concatMapM (rangeToSelection blockId) ranges
    liftIO $ Sync.setImProgress uiChan sels

updateRenderingRanges :: Cmd.M m => BlockId -> [TrackId]
    -> ScoreT.Instrument -> (RealTime, RealTime)
    -> m [(TrackId, (RealTime, RealTime))]
updateRenderingRanges blockId trackIds instrument range = do
    tracks <- Cmd.gets $ Map.findWithDefault mempty blockId
        . Cmd.stateImProgress . Cmd.statePlay
    let new = Map.fromList
            [(trackId, Map.singleton instrument range) | trackId <- trackIds]
    let merged = Map.unionWith (<>) new tracks
    Cmd.modifyPlayState $ \st -> st
        { Cmd.stateImProgress =
            Map.insert blockId merged $ Cmd.stateImProgress st
        }
    pure $ trackRanges merged trackIds

trackRanges :: Map TrackId (Map ScoreT.Instrument (RealTime, RealTime))
    -> [TrackId] -> [(TrackId, (RealTime, RealTime))]
trackRanges tracks = mapMaybe (traverse (expand . Map.elems)) . mapMaybe get
    where
    get trackId = (trackId,) <$> Map.lookup trackId tracks
    expand [] = Nothing
    expand ranges = Just (minimum (map fst ranges), maximum (map snd ranges))

-- | Convert a track level RealTime range to lower level per-view TrackTime
-- ranges.
rangeToSelection :: Cmd.M m => BlockId -> (TrackId, (RealTime, RealTime))
    -> m [((BlockId, TrackId), ((ScoreTime, ScoreTime), Color.Color))]
rangeToSelection blockId (trackId, (start, end)) = do
    invTempo <- Perf.getInverseTempo blockId
    pure $ fromMaybe [] $ do
        start <- toScore invTempo blockId trackId start
        end <- toScore invTempo blockId trackId end
        pure $ map ((blockId, trackId),)
            [ ((start, end), Config.imWorkingColor)
            , ((end, endOfTrack), Config.imPendingColor)
            ]

endOfTrack :: TrackTime
endOfTrack = 99999 -- TODO use a special to-end value?

toScore :: Transport.InverseTempoFunction -> BlockId -> TrackId -> RealTime
    -> Maybe ScoreTime
toScore invTempo blockId trackId =
    lookup trackId <=< lookup blockId . invTempo Transport.NoStop

-- ** track signals

-- | This takes the Ui.State from 'Cmd.perf_ui_state' because the current
-- Ui.State may have changes which haven't yet been synced to the UI.
updateTrackSignals :: Fltk.Channel -> BlockId -> Ui.State
    -> Track.TrackSignals -> IO ()
updateTrackSignals uiChan blockId state tsigs =
    case renderingTracks blockId state of
        -- This means a bad BlockId or bug in rendering_tracks.
        Left err -> Log.error $ pretty err
        Right tracks -> Sync.setTrackSignals uiChan $
            [ (viewId, tracknum, if wantsTsig
                then Map.findWithDefault empty (blockId, trackId) tsigs
                else empty)
            | (viewId, trackId, tracknum, wantsTsig) <- tracks
            ]
    where
    -- If there's no recorded signal, I send an empty one, to make sure that if
    -- there used to be one I will clear it out.  This is because "removed an
    -- existing track signal" and "never had a track signal" look the same
    -- from here, and always sending an empty seems less error-prone than
    -- trying to figure out the distinction.
    empty = Track.emptyTrackSignal

-- | Get the tracks of this block and whether they want to render a signal.
renderingTracks :: BlockId -> Ui.State
    -> Either Ui.Error [(ViewId, TrackId, TrackNum, Bool)]
renderingTracks blockId state = Ui.eval state $ do
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    blocks <- mapM Ui.blockOf viewIds
    btracks <- mapM getTracks blocks
    pure $ do
        (viewId, tracks) <- zip viewIds btracks
        ((tracknum, trackId, flags), track) <- tracks
        -- I don't want to send even an empty signal to these.
        guard $ not $ Block.isCollapsed flags
        pure
            ( viewId, trackId, tracknum
            , Block.trackWantsSignal flags track
            )
    where
    getTracks block = zip triples <$> mapM Ui.getTrack trackIds
        where
        trackIds = [tid | (_, tid, _) <- triples]
        triples =
            [ (tracknum, tid, Block.trackFlags track)
            | (tracknum,
                track@(Block.Track { Block.tracklikeId = Block.TId tid _ }))
                <- zip [0..] (Block.blockTracks block)
            ]

-- ** highlights

-- | Get highlights from the events, clear old highlights, and set the new ones.
updateHighlights :: Fltk.Channel -> BlockId -> Vector.Vector Score.Event
    -> Cmd.CmdT IO ()
updateHighlights uiChan blockId events = do
    sels <- getEventHighlights blockId events
    viewIds <- Map.keysSet <$> Ui.viewsOf blockId
    let usedViewIds = Set.fromList [viewId | ((viewId, _), _) <- sels]
    liftIO $ do
        Sync.clearHighlights uiChan $
            Set.toList $ viewIds `Set.difference` usedViewIds
        Sync.setHighlights uiChan sels

-- | Get highlight selections from the events.
getEventHighlights :: Cmd.M m => BlockId
    -- ^ only get highlights for events on this block
    -> Vector.Vector Score.Event
    -> m [((ViewId, TrackNum), (Range, Color.Color))]
getEventHighlights blockId events = do
    colors <- Cmd.gets $ Cmd.configHighlightColors . Cmd.stateConfig
    resolveTracks $ eventHighlights blockId colors events

resolveTracks :: Ui.M m => [((BlockId, TrackId), a)]
    -> m [((ViewId, TrackNum), a)]
resolveTracks = concatMapM resolve
    where
    resolve ((blockId, trackId), val) = do
        tracknum <- Ui.getTracknumOf blockId trackId
        viewIds <- Map.keys <$> Ui.viewsOf blockId
        pure [((viewId, tracknum), val) | viewId <- viewIds]

eventHighlights :: BlockId -> Map Color.Highlight Color.Color
    -> Vector.Vector Score.Event
    -> [((BlockId, TrackId), (Range, Color.Color))]
eventHighlights derivedBlockId colors
    | Map.null colors = const []
    | otherwise = Lists.uniqueOn key . Vector.foldr collect []
    where
    key (track, (range, _)) = (track, range)
    collect event accum
        | highlight /= Color.NoHighlight,
                Just (blockId, trackId, range) <- maybePos,
                blockId == derivedBlockId,
                Just color <- Map.lookup highlight colors =
            ((blockId, trackId), (range, color)) : accum
        | otherwise = accum
        where
        highlight = Score.eventHighlight event
        maybePos = Stack.blockTrackRegionOf $ Score.eventStack event

-- * play

-- | This actually kicks off a MIDI play thread, and if an inverse tempo
-- function is given, a play monitor thread.
play :: Fltk.Channel -> Ui.State -> Transport.Info
    -> Cmd.PlayArgs -> IO Transport.PlayControl
play uiChan uiState transportInfo
        (Cmd.PlayArgs mmc name midiMsgs scMsgs maybeInvTempo repeatAt
            imEnd playImDirect) = do
    playCtl <- Transport.playControl
    players <- Transport.activePlayers
    let midiState = Midi.Play.State
            { _playControl = playCtl
            , _players = players
            , _info = transportInfo
            , _imEnd = imEnd
            }
    Midi.Play.play midiState mmc name midiMsgs repeatAt
    unless (null (Sc.Note.notes scMsgs)) $ do
        let scState = Sc.Play.State
                { _playControl = playCtl, _players = players }
        Sc.Play.play scState scMsgs repeatAt
    -- Pass the current state in the MVar.  ResponderSync will keep it up
    -- to date afterwards, but only if blocks are added or removed.
    MVar.modifyMVar_ (Transport.infoState transportInfo) $
        const (pure uiState)
    whenJust playImDirect $ \args -> do
        Transport.playerStarted players
        void $ Thread.start $ playImDirectThread playCtl args
            `Exception.finally` Transport.playerStopped players
    Thread.start $ case maybeInvTempo of
        Just invTempo -> do
            state <- monitorState uiChan transportInfo playCtl players
                invTempo repeatAt
            playMonitorThread state
        Nothing -> pure ()
    Thread.start $ waitForPlayers playCtl
        (Transport.infoSendStatus transportInfo) players
    pure playCtl

waitForPlayers :: Transport.PlayControl -> (Transport.Status -> IO ())
    -> Transport.ActivePlayers -> IO ()
waitForPlayers playCtl sendStatus players = do
    sendStatus Transport.Playing
    Transport.waitPlayerStopped players
    sendStatus $ Transport.Stopped playCtl

-- | Start streaming audio from the given start time, until the PlayControl
-- says to stop.
playImDirectThread :: Transport.PlayControl -> Cmd.PlayDirectArgs -> IO ()
#ifdef ENABLE_IM
playImDirectThread (Transport.PlayControl quit)
        (Cmd.PlayDirectArgs scorePath blockId muted start) =
    StreamAudio.play verbose Nothing quit scorePath blockId muted start
#else
playImDirectThread _ _ =
    errorIO "can't play_im_direct_thread when im is not linked in"
#endif
    where verbose = False

-- | Run along the InverseTempoMap and update the play position selection.
-- Note that this goes directly to the UI through Sync, bypassing the usual
-- state diff folderol.
playMonitorThread :: MonitorState -> IO ()
playMonitorThread state = do
    uiState <- MVar.readMVar (monitorUiState state)
    Sync.clearPlayPosition (monitorUiChannel state) $
        Map.keys $ Ui.stateViews uiState
    monitorLoop state

monitorState :: Fltk.Channel -> Transport.Info
    -> Transport.PlayControl
    -> Transport.ActivePlayers
    -> Transport.InverseTempoFunction
    -> Maybe RealTime
    -> IO MonitorState
monitorState uiChan transportInfo playCtl players invTempoFunc
        repeatAt = do
    let getNow = Transport.infoGetCurrentTime transportInfo
    -- This won't be exactly the same as the renderer's ts offset, but
    -- it's probably close enough.
    offset <- getNow
    pure $ MonitorState
        { monitorPlayers = players
        , monitorPlayCtl = playCtl
        , monitorOffset = offset
        , monitorGetNow = getNow
        , monitorInvTempoFunc = invTempoFunc
        , monitorActiveSels = Set.empty
        , monitorUiState = Transport.infoState transportInfo
        , monitorRepeatAt = repeatAt
        , monitorUiChannel = uiChan
        }

data MonitorState = MonitorState
    { monitorPlayers :: !Transport.ActivePlayers
    , monitorPlayCtl :: !Transport.PlayControl
    -- | When the monitor thread started.
    , monitorOffset :: !RealTime
    , monitorGetNow :: !(IO RealTime)
    , monitorInvTempoFunc :: !Transport.InverseTempoFunction
    , monitorActiveSels :: !(Set (ViewId, [TrackNum]))
    , monitorUiState :: !(MVar.MVar Ui.State)
    , monitorRepeatAt :: !(Maybe RealTime)
    , monitorUiChannel :: !Fltk.Channel
    }

monitorLoop :: MonitorState -> IO ()
monitorLoop state = do
    -- This is play time, relative to when play started.  It is not RealTime
    -- position in the score, because we might have started in the middle.
    -- The inv_tempo_func must have had this offset already applied.
    now <- maybe id (flip Num.fmod) (monitorRepeatAt state)
        . subtract (monitorOffset state) <$> monitorGetNow state
    let fail err = Log.error ("state error in play monitor: " <> showt err)
            >> pure []
    uiState <- MVar.readMVar (monitorUiState state)
    let blockPos = monitorInvTempoFunc state Transport.StopAtEnd now
    playPos <- fmap extendToTrack0 $ either fail pure $
        Ui.eval uiState $ Perf.blockPosToPlayPos blockPos
    Sync.setPlayPosition (monitorUiChannel state) playPos

    let activeSels = Set.fromList
            [(viewId, map fst numPos) | (viewId, numPos) <- playPos]
    Sync.clearPlayPosition (monitorUiChannel state) $ map fst $ Set.toList $
        Set.difference (monitorActiveSels state) activeSels
    state <- pure $ state { monitorActiveSels = activeSels }

    done <- orM
        -- out of score
        -- If repeat_at is on, then the players will never stop on their own,
        -- even if I run out of tempo map (which happens if the repeat point is
        -- at or past the end of the score).  When the monitor thread stops, it
        -- sends Stopped on the transport, which clears out the player ctl
        -- which will make the player unstoppable, if it's still going.
        [ pure $ null blockPos && Maybe.isNothing (monitorRepeatAt state)
        -- stop requested
        , Transport.pollStopPlayer 0 (monitorPlayCtl state)
        -- all players stopped, after this Transport.Stopped is emitted,
        -- Cmd.state_play_control is cleared, and it's no longer possible to
        -- do a Transport.stop_player.
        , Transport.pollPlayerStopped 0 (monitorPlayers state)
        ]
    if done
        then Sync.clearPlayPosition (monitorUiChannel state) $ map fst $
            Set.toList (monitorActiveSels state)
        else Thread.delay 0.05 >> monitorLoop state

-- | If there's a playback on track 1 but not on track 0, then track 0 is
-- probably a ruler.  If I put a playback there too, then the playback position
-- will be more obvious on a narrow block.
extendToTrack0 :: [(ViewId, [(TrackNum, ScoreTime)])]
    -> [(ViewId, [(TrackNum, ScoreTime)])]
extendToTrack0 = map (second extend)
    where
    extend ((tracknum, t) : tracks)
        | tracknum == 1 = (0, t) : (1, t) : tracks
    extend tracks = tracks
