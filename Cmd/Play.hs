-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Master control for playing blocks.

    Here's how it works:

    - Find the relevant block to play.

    - Deriver generates a performable score and an inverse tempo map.

    - The score is preprocessed by adding the current absolute time to it
    and skipping notes based on the start offset.

    - The score is sent to the Performer, which starts
    'Perform.Midi.Play.player_thread' and returns a
    'Transport.PlayControl' and a 'Transport.PlayMonitorControl'.  The
    PlayControl is just to stop the player_thread, and the PlayMonitorControl
    is so the player_thread can signal that it's done.

    - The PlayMonitorControl and tempo map are passed to
    'Cmd.PlayC.play_monitor_thread', which uses the tempo map to display the
    play position in the various blocks.  It stops when it runs out of tempo
    map (which corresponds with running off the end of the score), or the
    player_thread sends a stop.  It's not synchronized to the playback in any
    way, but of course they are both working from the same score.

    - A stop from the user sets 'Transport.stop_player', which causes the
    player_thread to quit, which in turn tells the monitor thread.

    So, there are three threads involved: the player_thread is scheduling MIDI
    msgs, the play_monitor_thread sweeps the play position along, and the app
    event handler is waiting for events in the responder.

    The player_thread also gets the event loopback channel in
    'Transport.info_send_status', so it can send 'Cmd.Msg.Transport' msgs
    through the event loop.  They're picked up by 'Cmd.PlayC.cmd_play_msg',
    which can use them to set UI state like changing the play box color and
    logging.

    The play_monitor_thread is kicked off simultaneously with the
    player_thread, and advances the play selection in its own loop, using the
    tempo map from the deriver.  It will keep running until it gets a stop msg
    from the control or the tempo map tells it there is no more score to
    \"play\".  While the monitor doesn't actually play anything, it's the one
    that sends Playing and Stopped transport msgs to indicate player status.
    If all goes well, the monitor and the player will run to completion, the
    monitor will send Stopped, and the player will exit on its own.

    The im backend complicates things a bit.  See NOTE [play-im].

    For example:

    In a normal situation, the player will do its thing and the monitor will
    eventually run out of InverseTempoMap, which will return Nothing.  The
    monitor will send Stopped, which will clear the player control from the
    responder Cmd.State, which is how the UI knows whether playing is in
    progress.  The player is assumed to have completed and exited on its own,
    probably even before the playback audio is completed, since it schedules in
    advance.

    If the player dies on an error, it sends a Died to the responder chan.  As
    mentioned above, it will also tell the monitor to stop.  The monitor will
    notice this, and may stop itself, emitting a Stopped msg.  The Stopped msg
    will then cause the responder to clear the player control out of its state,
    which lets it know that play has stopped and it's ok to start another play
    thread.

    If the user requests a stop, the responder sets the player control to Stop.
    The player stops, telling the monitor to stop, which emits Stopped, which
    clears the PlayMonitorControl.
-}
module Cmd.Play where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq
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
import qualified Perform.Transport as Transport

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Shared.Config
import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Zoom as Zoom

import           Global
import           Types


modify_play_multiplier :: Cmd.M m => (RealTime -> RealTime) -> m ()
modify_play_multiplier f = do
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_play_multiplier = to_1 $ f (Cmd.state_play_multiplier st) }
    whenM has_im $ Ui.update_all -- TODO: why?  For waveform?
    where
    -- Set to 1 if I'm this close.  Otherwise repeated multiplies don't
    -- necessarily come back exactly.
    to_1 val
        | abs (1 - val) <= threshold = 1
        | otherwise = val
    threshold = 0.01

-- * stop

-- | Context sensitive stop that stops whatever is going on.  First it stops
-- realtime play, then step play, and then it just sends all notes off.  If
-- it does the last one, it returns False in case you want to go stop something
-- else.
cmd_context_stop :: Cmd.CmdT IO Bool
cmd_context_stop = gets Cmd.state_play_control >>= \case
    Just ctl -> stop ctl >> return True
    Nothing -> do
        step_playing <- Cmd.gets $
            Maybe.isJust . Cmd.state_step . Cmd.state_play
        if step_playing
            then StepPlay.cmd_clear >> return True
            -- play_cache may still be streaming after the karya transport
            -- stops.
            else Cmd.all_notes_off >> stop_im >> return False

cmd_stop :: Cmd.CmdT IO Cmd.Status
cmd_stop = do
    maybe_ctl <- gets Cmd.state_play_control
    whenJust maybe_ctl stop
    return Cmd.Done

stop :: Transport.PlayControl -> Cmd.CmdT IO ()
stop ctl = do
    liftIO $ Transport.stop_player ctl
    stop_im

-- | Stop im stream, if playing.  See NOTE [play-im].
stop_im :: Cmd.CmdT IO ()
stop_im = whenJustM im_addr $ \(wdev, chan) ->
    Cmd.midi wdev $ Midi.ChannelMessage chan Im.Play.stop

im_addr :: Cmd.M m => m (Maybe Patch.Addr)
im_addr = do
    allocs <- Ui.config#Ui.allocations_map <#> Ui.get
    return $ case lookup_im_config allocs of
        Right (_, addr) -> Just addr
        Left _ -> Nothing

has_im :: Cmd.M m => m Bool
has_im = Maybe.isJust <$> im_addr

-- * play

-- | Play the local block from its beginning.
local_block :: Cmd.M m => m Cmd.PlayMidiArgs
local_block = do
    block_id <- Cmd.get_focused_block
    from_score block_id Nothing 0 Nothing

-- | Start playing from the point selection on the local block.  If the
-- selection is a range, loop that range forever.
local_selection :: Cmd.M m => m Cmd.PlayMidiArgs
local_selection = do
    (block_id, _, track_id, _) <- Selection.get_insert
    sel <- Selection.get
    let (pos, repeat_at) = if Sel.is_point sel
            then (Sel.start_pos sel, Nothing)
            else Just <$> Sel.range sel
    from_score block_id (Just track_id) pos repeat_at

-- | Play the current block's performance from the previous
-- 'Cmd.state_play_step'.
local_previous :: Cmd.M m => m Cmd.PlayMidiArgs
local_previous = do
    step <- gets Cmd.state_play_step
    (block_id, tracknum, track_id, pos) <- Selection.get_insert
    prev <- TimeStep.rewind step block_id tracknum pos
    local_from block_id track_id (fromMaybe 0 prev)

-- | Play the current block's performance from the top of the window.
local_top :: Cmd.M m => m Cmd.PlayMidiArgs
local_top = do
    (block_id, track_id, top) <- top_of_block
    local_from block_id track_id top

local_from :: Cmd.M m => BlockId -> TrackId -> TrackTime -> m Cmd.PlayMidiArgs
local_from block_id track_id pos =
    from_score block_id (Just track_id) pos Nothing

-- | Play the root block from its beginning.
root_block :: Cmd.M m => m Cmd.PlayMidiArgs
root_block = do
    Ui.lookup_root_id >>= \case
        Nothing -> local_block
        Just root_id -> from_score root_id Nothing 0 Nothing

-- | Play the root performance from the selection on the root block.  This
-- is useful to manually set a point to start playing.
root_from_root_selection :: Cmd.M m => m Cmd.PlayMidiArgs
root_from_root_selection = do
    (block_id, _, track_id, pos) <- Selection.get_root_insert
    from_score block_id (Just track_id) pos Nothing

-- | The same as 'local_selection', but use the root performance.
root_selection :: Cmd.M m => m Cmd.PlayMidiArgs
root_selection = do
    (block_id, _, track_id, _) <- Selection.get_insert
    sel <- Selection.get
    let (pos, repeat_at)
            | Sel.is_point sel = (Sel.start_pos sel, Nothing)
            | otherwise = Just <$> Sel.range sel
    Ui.lookup_root_id >>= \case
        Nothing -> local_selection
        Just root_id -> do
            perf <- get_performance root_id
            let realtime_at = Perf.lookup_realtime perf block_id (Just track_id)
            real_repeat_at <- maybe (return Nothing) realtime_at repeat_at
            maybe local_selection (from_realtime root_id real_repeat_at)
                =<< realtime_at pos

-- | Find the previous step on the focused block, get its RealTime, and play
-- from the root at that RealTime.  If this block isn't linked from the root,
-- then fall back on 'local_previous'.
root_previous :: Cmd.M m => m Cmd.PlayMidiArgs
root_previous = do
    (block_id, tracknum, track_id, pos) <- Selection.get_insert
    step <- gets Cmd.state_play_step
    prev <- fromMaybe pos <$> TimeStep.rewind step block_id tracknum pos
    root_from block_id track_id prev

-- | Like 'root_previous', but play from the top of the selected block.
root_top :: Cmd.M m => m Cmd.PlayMidiArgs
root_top = do
    (block_id, track_id, top) <- top_of_block
    root_from block_id track_id top

top_of_block :: Cmd.M m => m (BlockId, TrackId, TrackTime)
top_of_block = do
    (block_id, _, track_id, _) <- Selection.get_insert
    view_id <- Cmd.get_focused_view
    top <- Zoom.offset . Block.view_zoom <$> Ui.get_view view_id
    return (block_id, track_id, top)

root_from :: Cmd.M m => BlockId -> TrackId -> TrackTime -> m Cmd.PlayMidiArgs
root_from block_id track_id pos = do
    play_root <- maybe_root_from block_id track_id pos
    maybe (local_from block_id track_id pos) return play_root

maybe_root_from :: Cmd.M m => BlockId -> TrackId -> ScoreTime
    -> m (Maybe Cmd.PlayMidiArgs)
maybe_root_from block_id track_id pos =
    justm Ui.lookup_root_id $ \root_id -> do
        perf <- get_performance root_id
        justm (Perf.lookup_realtime perf block_id (Just track_id) pos) $
            \start -> Just <$> from_realtime root_id Nothing start

from_score :: Cmd.M m => BlockId
    -> Maybe TrackId -- ^ Track to play from.  Since different tracks can have
    -- different tempos, a track is needed to convert to RealTime.  If not
    -- given, use the first track that has tempo information.
    -> ScoreTime -- ^ Convert to RealTime and start playing from this time.
    -> Maybe ScoreTime
    -> m Cmd.PlayMidiArgs
from_score block_id start_track start_pos repeat_at = do
    start <- get_realtime block_id block_id start_track start_pos
    repeat_at <- maybe (return Nothing)
        (fmap Just . get_realtime block_id block_id start_track) repeat_at
    from_realtime block_id repeat_at start

get_realtime :: Cmd.M m => BlockId
    -- ^ Lookup realtime according to the performance of this block.
    -> BlockId
    -- ^ Lookup realtime at the position (TrackId, ScoreTime) within this block.
    -> Maybe TrackId -> ScoreTime
    -> m RealTime
get_realtime perf_block play_block maybe_track_id pos = do
    perf <- get_performance perf_block
    maybe_start <- Perf.lookup_realtime perf play_block maybe_track_id pos
    case maybe_start of
        Nothing -> Cmd.throw $ "play " <> showt perf_block
            <> " has no tempo information"
        Just start -> return start

get_performance :: Cmd.M m => BlockId -> m Cmd.Performance
get_performance block_id = do
    perf <- Cmd.require ("no performance for block " <> showt block_id)
        =<< lookup_current_performance block_id
    write_logs block_id perf
    return perf

write_logs :: Cmd.M m => BlockId -> Cmd.Performance -> m ()
write_logs block_id perf = unless (Cmd.perf_logs_written perf) $ do
    -- There are so many cache msgs it clogs up logview.  I'm writing a summary
    -- anyway so I can filter them out.
    mapM_ Log.write $ filter (not . Cache.is_cache_log) (Cmd.perf_logs perf)
    -- Logview can only display one set of stats, so only show the root block.
    whenM ((== Just block_id) <$> Ui.lookup_root_id) $
        record_cache_stats (Cmd.perf_logs perf)
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_current_performance = Map.insert block_id
            (perf { Cmd.perf_logs_written = True })
            (Cmd.state_current_performance st)
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
record_cache_stats :: Cmd.M m => [Log.Msg] -> m ()
record_cache_stats logs = do
    let (rederived, cached) = extract_cache_stats get_block_id logs
    Cmd.set_global_status "~C" $ "[" <> showt (length cached) <> " / "
        <> showt (Num.sum (map snd cached)) <> "] "
        <> elide (Text.unwords (map (Id.ident_name . fst) cached))
    status_keys <- Cmd.gets (Map.keysSet . Cmd.state_global_status)
    let keys = map (("~X "<>) . fst) rederived
        gone = Set.filter ("~X " `Text.isPrefixOf`) $
            status_keys Set.\\ Set.fromList keys
    forM_ (zip keys (map snd rederived)) $ \(key, block_ids) ->
        Cmd.set_global_status key $ "[" <> showt (length block_ids) <> "] "
            <> elide (Text.unwords (map Id.ident_name block_ids))
    forM_ (Set.toList gone) $ \key -> Cmd.set_global_status key ""
    where
    max_chars = 45
    elide s
        | Text.length s > max_chars = Text.take (max_chars-3) s <> "..."
        | otherwise = s

extract_cache_stats :: (Log.Msg -> Maybe k) -> [Log.Msg]
    -> ([(Text, [k])], [(k, Int)])
    -- ^ (cache misses, cache hits):
    -- ([(because, [key])], [(key, cached_vals)])
extract_cache_stats key logs = (rederived, cached)
    where
    -- [("because xyz", [bid, bid, bid, ...])]
    rederived = map (second (map fst)) $ Seq.keyed_group_sort snd
        [(block_id, because) | (block_id, Left because) <- stats]
    -- [(bid1, 42), (bid2, 32), ...]
    cached = [(block_id, vals) | (block_id, Right vals) <- stats]
    stats = mapMaybe extract logs
    extract log = case key log of
        Nothing -> Nothing
        Just block_id
            | Just because <- Cache.cache_miss_reason log ->
                Just (block_id, Left because)
            | Just vals <- Cache.cache_hit_events log ->
                Just (block_id, Right vals)
            | otherwise -> Nothing

-- | Get block cache stats.
get_block_id :: Log.Msg -> Maybe BlockId
get_block_id = Stack.block_of <=< Seq.head . Stack.innermost <=< Log.msg_stack

-- | Get track cache stats.
get_track_id :: Log.Msg -> Maybe (BlockId, TrackId)
get_track_id = Stack.block_track_of <=< Log.msg_stack

-- | Play the performance of the given block starting from the given time.
from_realtime :: Cmd.M m => BlockId -> Maybe RealTime -> RealTime
    -> m Cmd.PlayMidiArgs
from_realtime block_id repeat_at start_ = do
    -- Since 0 is considered "the beginning", even if that happens to be before
    -- 0, there's no point asking for something before 0, and will just cause
    -- play to seem to wedge for a moment.  'PlayUtil.perform_from' has
    -- a special hack to notice and include notes < 0.
    let start = max 0 start_
    play_control <- gets Cmd.state_play_control
    whenJust play_control $ \_ -> Cmd.throw "player already running"

    perf <- Cmd.require ("no performance for block " <> showt block_id)
        =<< lookup_current_performance block_id
    multiplier <- gets (recip . Cmd.state_play_multiplier)

    maybe_sync <- gets Cmd.state_sync
    case maybe_sync of
        -- Don't bother with a MMC Goto if I'm going to send MTC.
        Just sync | not (Cmd.sync_mtc sync) ->
            Cmd.midi (Cmd.sync_device sync) $ Selection.mmc_goto sync start
        _ -> return ()
    -- MTC rounds up to the previous whole frame, so the mtc might start
    -- slightly before the notes.
    -- TODO actually DAWs need a bit of time to sync, so maybe I should start
    -- further in advance.
    let mtc = PlayUtil.shift_messages 1 start $ map LEvent.Event $
            generate_mtc maybe_sync start

    allocs <- Ui.config#Ui.allocations_map <#> Ui.get
    (im_insts, play_cache_addr) <- case lookup_im_config allocs of
        Right (im_insts, play_cache_addr) ->
            return (im_insts, Just play_cache_addr)
        Left Nothing -> return (mempty, Nothing)
        Left (Just msg) -> Cmd.throw msg
    muted <- Perf.infer_muted_instruments
    score_path <- Cmd.gets Cmd.score_path
    let im_msgs = maybe []
            (im_play_msgs score_path block_id muted (start * multiplier))
            play_cache_addr

    msgs <- PlayUtil.perform_from start perf
    let adjust0 = get_adjust0 start (not (null im_msgs)) msgs
            (Cmd.perf_events perf)
    msgs <- return $ PlayUtil.shift_messages multiplier adjust0 msgs

    -- See doc for "Cmd.PlayC" for why I return a magic value.
    return $ Cmd.PlayMidiArgs
        { play_sync = maybe_sync
        , play_name = pretty block_id
        , play_midi = im_msgs ++ merge_midi msgs mtc
        , play_inv_tempo = Just $ \stop ->
            Cmd.perf_inv_tempo perf stop . (+adjust0) . (/multiplier)
        , play_repeat_at = (*multiplier) . subtract start <$> repeat_at
        , play_im_end = if Set.null im_insts then Nothing
            else Score.event_end <$> Util.Vector.find_end
                ((`Set.member` im_insts) . Score.event_instrument)
                (Cmd.perf_events perf)
        }

get_adjust0 :: RealTime -> Bool -> [LEvent.LEvent Midi.WriteMessage]
    -> Vector.Vector Score.Event -> RealTime
get_adjust0 start has_im msgs events = negative_start - im_latency
    where
    -- Events can wind up before 0, say if there's a grace note on a note at 0.
    -- To have them play correctly, perform_from will give me negative events
    -- when starting from 0, and then I have to shift the start time back to
    -- consider the first event the new 0.
    negative_start
        | start == 0 && fst_msg < 0 = fst_msg
        | otherwise = start
    -- The first MIDI msg will incorporate any possible leading controls.
    -- The first Score.Event should be after that, unless there's an im
    -- event at negative time.
    fst_msg = min first_score (PlayUtil.first_time msgs)
    first_score = maybe 0 Score.event_start $ Seq.head $ Vector.toList $
        Vector.take 1 events
    im_latency = if has_im
        then AUtil.toSeconds Shared.Config.startLatency else 0

lookup_im_config :: Map ScoreT.Instrument UiConfig.Allocation
    -> Either (Maybe Text) (Set ScoreT.Instrument, Patch.Addr)
lookup_im_config allocs = do
    when (Set.null im_insts) $ Left Nothing
    alloc <- justErr (Just "im allocations but no play-cache alloc, so they\
        \ won't play, allocate with LInst.add_play_cache") $
            List.find is_play_cache (Map.elems allocs)
    case UiConfig.alloc_backend alloc of
        UiConfig.Midi config -> case Patch.config_addrs config of
            [] -> Left $ Just $
                pretty Im.Play.qualified <> " allocation with no addrs"
            addr : _ -> return (im_insts, addr)
        _ -> Left $ Just $
            pretty Im.Play.qualified <> " with non-MIDI allocation"
    where
    is_play_cache = (==Im.Play.qualified) . UiConfig.alloc_qualified
    im_insts = Set.fromList $ map fst $
        filter (UiConfig.is_im_allocation . snd) $ Map.toList allocs

-- | If there are im instruments, find the 'Im.Play.play_cache_synth'
-- allocation.
lookup_play_cache_addr :: Cmd.M m => m (Maybe Patch.Addr)
lookup_play_cache_addr = do
    allocs <- Ui.config#Ui.allocations_map <#> Ui.get
    let is_play_cache = (==Im.Play.qualified) . UiConfig.alloc_qualified
    if not (any (UiConfig.is_im_allocation) allocs)
        then return Nothing
        else case List.find is_play_cache (Map.elems allocs) of
            Nothing -> do
                Log.warn $ "im allocations but no play-cache, so they won't\
                    \play, allocate with LInst.add_play_cache"
                return Nothing
            Just alloc -> case UiConfig.alloc_backend alloc of
                UiConfig.Midi config -> case Patch.config_addrs config of
                    [] -> Ui.throw $
                        pretty Im.Play.qualified <> " allocation with no addrs"
                    addr : _ -> return $ Just addr
                _ -> Ui.throw $
                        pretty Im.Play.qualified <> " with non-MIDI allocation"

im_play_msgs :: FilePath -> BlockId -> Set ScoreT.Instrument -> RealTime
    -> Patch.Addr -> [LEvent.LEvent Midi.WriteMessage]
im_play_msgs score_path block_id muted start (wdev, chan) =
    zipWith msg ts $ Im.Play.encode_time start
        ++ Im.Play.encode_play_config score_path block_id muted
        ++ [Im.Play.start]
    where
    msg t = LEvent.Event . Midi.WriteMessage wdev t . Midi.ChannelMessage chan
    -- 'encode_time' includes the bit position so it doesn't depend on order,
    -- but encode_block does.  With CoreMIDI it seems msgs stay in order even
    -- when they have the same timestamp, I'll put on a timestamp just in case.
    -- They're all in the past, so they should still be "as fast as possible",
    -- and not 10ms, or whatever it winds up being.
    ts = map RealTime.milliseconds [0..]

-- | Merge a finite list of notes with an infinite list of MTC.
merge_midi :: [LEvent.LEvent Midi.WriteMessage]
    -> [LEvent.LEvent Midi.WriteMessage] -> [LEvent.LEvent Midi.WriteMessage]
merge_midi = merge_until (LEvent.either Midi.wmsg_ts (const 0))

-- | Merge until the leftmost list runs out.
merge_until :: Ord k => (a -> k) -> [a] -> [a] -> [a]
merge_until key = go
    where
    go xs [] = xs
    go [] _ = []
    go (x:xs) (y:ys)
        | key x <= key y = x : go xs (y:ys)
        | otherwise = y : go (x:xs) ys

generate_mtc :: Maybe Cmd.SyncConfig -> RealTime -> [Midi.WriteMessage]
generate_mtc (Just sync) start | Cmd.sync_mtc sync =
    map make $ (0, Midi.mtc_sync rate smpte) : Midi.generate_mtc rate frame
    where
    smpte = Midi.frame_to_smpte rate frame
    frame = Midi.seconds_to_frame rate (RealTime.to_seconds start)
    rate = Cmd.sync_frame_rate sync
    make (secs, msg) =
        Midi.WriteMessage (Cmd.sync_device sync) (RealTime.seconds secs) msg
generate_mtc _ _ = []

lookup_current_performance :: Cmd.M m => BlockId -> m (Maybe Cmd.Performance)
lookup_current_performance block_id =
    Map.lookup block_id <$> gets Cmd.state_current_performance

-- * implementation

gets :: Cmd.M m => (Cmd.PlayState -> a) -> m a
gets f = Cmd.gets (f . Cmd.state_play)


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
