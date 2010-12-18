{- | Master control for playing blocks.

    Here's how it works:

    - Find the relevant block to play.

    - Deriver generates a performable Score and an inverse tempo map.

    - The Score is preprocessed by adding the current absolute time to it
    and skipping notes based on the start offset.

    - The Score is sent to the Performer, which splits it up by backend,
    starts whatever processes are necessary to play the notes, and returns
    a transport control mutable that can be used to stop the playback.

    - The transport and tempo map are passed to a play display updater, which
    uses the tempo map to display the play position in the various blocks, and
    aborts along with the performer if the transport says to stop.  It's not
    synchronized to the playback in any way (which may be a separate process
    playing an audio file), but the fact that it's working from the same tempo
    map that generated the audio should keep it in sync.

    - There are three threads involved: the performer manages whatever process
    needs to perform the score, the updater sweeps the play position along, and
    the app event handler is waiting for events in the responder.

    - On cancel, kill the thread, and invoke a backend specific means to cancel
    outstanding notes (flush midi port, kill external performer, ...).

    The player returns controls to communicate with the player and the updater.
    If the responder sets the player control, the player will quit.  The player
    stopping causes it to set the updater control, which causes the updater to
    quit (if there are multiple players, the updater should wait for them all
    to quit).

    There's a third control, which is a channel given to the player by the
    responder.  Both the player and the updater use it to send transport msgs
    to the responder.  All the player sends is a Died msg which can be logged
    when the player as started and stopped.  Transport msgs wind up in
    'cmd_play_msg', which can use them to set UI state like changing the
    play box color and logging.

    The updater is kicked off simultaneously with the performer, and advances
    the play selection in its own loop, using the tempo map from the deriver.
    It will keep running until it gets a stop msg from the control or the tempo
    map tells it there is no more score to \"play\".  While the updater doesn't
    actually play anything, it's the one that sends Playing and Stopped
    transport msgs to indicate performer status.  This is because there may
    be multiple simultaneous performers that may complete at different times
    and the updater will only emit Stopped if all of them have finished.  If
    all goes well, the updater and the performer will run to completion, the
    updater will send Stopped, and the performer will exit on its own.

    With multiple backends, there will be multiple update controls, and the
    updater will need to monitor them all.

    For example:

    In a normal situation, the performer will do its thing and the updater will
    eventually run out of InverseTempoMap (which will return Nothing).  The
    updater will send Stopped, which will clear the player control from the
    responder Cmd.State, which is how the UI knows whether playing is in
    progress.  The performer is assumed to have completed and exited on its
    own, probably even before the playback audio is completed, since it likely
    schedules in advance.

    If the performer dies on an error, it sends a Died to the responder chan.
    As mentioned above, it will also tell the updater to stop.  The updater
    will notice this, and may stop itself, emitting a Stopped msg.  The Stopped
    msg will then cause the responder to clear the player control out of its
    state, which lets it know that play has stopped and it's ok to start
    another play thread.

    If the user requests a stop, the responder sets the player control to Stop.
    The player stops, telling the updater to stop, which emits Stopped, which
    clears the updater control.
-}
module Cmd.Play where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import Ui
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.State as State
-- This causes a bunch of modules to import BlockC.  Can I move the updater
-- stuff out?
import qualified Ui.Sync as Sync

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Transport as Transport
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Cache as Cache
import qualified Perform.Midi.Play as Midi.Play

import qualified App.Config as Config


-- * cmds

cmd_play_focused :: Transport.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_focused transport_info = do
    block_id <- Cmd.get_focused_block
    -- cmd_play wants to start with a track, so pick the first one.
    -- TODO: pick the first event track!
    block <- State.get_block block_id
    track_id <- Cmd.require $ Seq.at (Block.block_track_ids block) 0
    cmd_play transport_info block_id (track_id, 0)

cmd_play_from_insert :: Transport.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_from_insert transport_info = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    cmd_play transport_info block_id (track_id, pos)

cmd_play_from_previous_step :: Transport.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_from_previous_step transport_info = do
    step <- Cmd.gets Cmd.state_play_step
    (block_id, tracknum, track_id, pos) <- Selection.get_insert
    prev <- TimeStep.step_from step TimeStep.Rewind block_id tracknum pos
    cmd_play transport_info block_id (track_id, (maybe 0 id prev))

cmd_play_from_previous_root_step :: Transport.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_from_previous_root_step transport_info = do
    (block_id, tracknum, track_id, pos) <- Selection.get_root_insert
    step <- Cmd.gets Cmd.state_play_step
    prev <- TimeStep.step_from step TimeStep.Rewind block_id tracknum pos
    cmd_play transport_info block_id (track_id, (maybe 0 id prev))

cmd_play :: Transport.Info -> BlockId -> (TrackId, ScoreTime)
    -> Cmd.CmdT IO Cmd.Status
cmd_play transport_info block_id (start_track, start_pos) = do
    cmd_state <- Cmd.get_state
    case Cmd.state_play_control cmd_state of
        Just _ -> Cmd.throw "player already running"
        _ -> return ()
    perf <- get_performance block_id

    -- Yes I already logged these, but that may have been buried.
    let warn_logs = filter ((>=Log.Warn) . Log.msg_prio) (Cmd.perf_logs perf)
        warn_text = if null warn_logs
            then "But there were no warnings which is odd."
            else "Warnings deriving: "
                ++ Seq.join ", " (map Log.msg_string warn_logs)
    start_ts <- case Cmd.perf_tempo perf block_id start_track start_pos of
        [] -> Cmd.throw $ show block_id ++ " has no tempo information, so it "
            ++ "probably failed to derive.  " ++ warn_text
        realtime : _ -> return (Timestamp.from_real_time realtime)
    let msgs = Cache.messages_from start_ts (Cmd.perf_midi_cache perf)
    (play_ctl, updater_ctl) <- Trans.liftIO $
        Midi.Play.play transport_info block_id msgs

    ui_state <- State.get
    Trans.liftIO $ Thread.start_thread "play position updater" $ updater_thread
        updater_ctl transport_info (Cmd.perf_inv_tempo perf) start_ts ui_state

    Cmd.modify_state $ \st -> st { Cmd.state_play_control = Just play_ctl }
    return Cmd.Done

cmd_stop :: Cmd.CmdT IO Cmd.Status
cmd_stop = do
    ctl <- Cmd.get_state >>= maybe (Cmd.throw "player thread not running")
        return . Cmd.state_play_control
    Trans.liftIO $ Transport.stop_player ctl
    return Cmd.Done

-- | Respond to msgs about derivation and playing status.
cmd_play_msg :: Msg.Msg -> Cmd.CmdT IO Cmd.Status
cmd_play_msg msg = do
    case msg of
        Msg.Transport (Transport.Status _ status) -> transport_msg status
        Msg.DeriveStatus block_id status -> derive_status_msg block_id status
        _ -> Cmd.abort
    return Cmd.Done
    where
    transport_msg status = case status of
        Transport.Playing -> return ()
        -- Either the performer has declared itself stopped, or the updater
        -- has declared it stopped.  In any case, I don't need a transport
        -- to tell it what to do anymore.
        Transport.Stopped -> Cmd.modify_state $ \st ->
            st { Cmd.state_play_control = Nothing }
        Transport.Died err_msg -> Log.warn ("player died: " ++ err_msg)
    derive_status_msg block_id status = do
        State.set_play_box block_id (derive_status_color status)
        case status of
            Msg.DeriveComplete track_signals -> do
                ui_state <- State.get
                Trans.liftIO $ Sync.set_track_signals ui_state track_signals
            _ -> return ()
    derive_status_color status = case status of
        Msg.StartedDeriving -> Config.busy_color
        Msg.Deriving -> Color.brightness 1.5 Config.busy_color
        Msg.DeriveFailed -> Config.warning_color
        Msg.DeriveComplete _ -> Config.box_color

-- * implementation

get_performance :: (Monad m) => BlockId -> Cmd.CmdT m Cmd.Performance
get_performance block_id = do
    threads <- Cmd.gets Cmd.state_performance_threads
    case Map.lookup block_id threads of
        Nothing -> State.throw $ "no performance for block " ++ show block_id
        Just pthread -> return $ Cmd.pthread_perf pthread


-- ** updater

-- | Run along the InverseTempoMap and update the play position selection.
-- Note that this goes directly to the UI through Sync, bypassing the usual
-- state diff folderol.
updater_thread :: Transport.UpdaterControl -> Transport.Info
    -> Transport.InverseTempoFunction -> Timestamp.Timestamp -> State.State
    -> IO ()
updater_thread ctl transport_info inv_tempo_func start_ts ui_state = do
    -- Send Playing and Stopped msgs to the responder for all visible blocks.
    let block_ids = Seq.unique $ Map.elems (Map.map Block.view_block
            (State.state_views ui_state))
        get_cur_ts = Transport.info_get_current_timestamp transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    ts_offset <- get_cur_ts
    let state = UpdaterState ctl (ts_offset - start_ts) get_cur_ts
            inv_tempo_func Set.empty ui_state
    let send status bid = Transport.info_send_status transport_info bid status
    Exception.bracket_
        (mapM_ (send Transport.Playing) block_ids)
        (mapM_ (send Transport.Stopped) block_ids)
        (updater_loop state)

data UpdaterState = UpdaterState {
    updater_ctl :: Transport.UpdaterControl
    , updater_ts_offset :: Timestamp.Timestamp
    , updater_get_cur_ts :: IO Timestamp.Timestamp
    , updater_inv_tempo_func :: Transport.InverseTempoFunction
    , updater_active_sels :: Set.Set (ViewId, [TrackNum])
    , updater_ui_state :: State.State
    }

updater_loop :: UpdaterState -> IO ()
updater_loop state = do
    cur_ts <- fmap (subtract (updater_ts_offset state))
        (updater_get_cur_ts state)

    let block_pos = updater_inv_tempo_func state cur_ts
    play_pos <- either
        (\err -> Log.error ("state error in updater: " ++ show err)
            >> return [])
        return
        (State.eval (updater_ui_state state) (block_pos_to_play_pos block_pos))
    Sync.set_play_position play_pos

    let active_sels = Set.fromList
            [(view_id, map fst num_pos) | (view_id, num_pos) <- play_pos]
    mapM_ Sync.clear_play_position $ map fst $
        Set.toList (Set.difference (updater_active_sels state) active_sels)
    state <- return $ state { updater_active_sels = active_sels }

    stopped <- Transport.check_player_stopped (updater_ctl state)
    if stopped || null block_pos
        then mapM_ Sync.clear_play_position $
            map fst (Set.toList (updater_active_sels state))
        else Thread.delay 0.05 >> updater_loop state


-- | Do all the annoying shuffling around to convert the deriver-oriented
-- blocks and tracks to the view-oriented views and tracknums.
block_pos_to_play_pos :: (State.UiStateMonad m) =>
    [(BlockId, [(TrackId, ScoreTime)])]
    -> m [(ViewId, [(TrackNum, Maybe ScoreTime)])]
block_pos_to_play_pos block_pos = fmap concat (mapM convert block_pos)

convert :: (State.UiStateMonad m) =>
    (BlockId, [(TrackId, ScoreTime)])
    -> m [(ViewId, [(TrackNum, Maybe ScoreTime)])]
convert (block_id, track_pos) = do
    view_ids <- fmap Map.keys (State.get_views_of block_id)
    block <- State.get_block block_id
    let tracknum_pos = concatMap (tracknums_of block) track_pos
    return [(view_id, tracknum_pos) | view_id <- view_ids]

tracknums_of :: Block.Block -> (TrackId, ScoreTime)
    -> [(TrackNum, Maybe ScoreTime)]
tracknums_of block (track_id, pos) =
    [ (tracknum, Just pos)
    | (tracknum, Block.TId tid _) <- zip [0..] (Block.block_tracklike_ids block)
    , tid == track_id ]


-- * util

-- | Find the block to play, relative to the given view.
-- find_play_block :: State.State -> ViewId -> BlockId
find_play_block view_id = do
    view <- State.get_view view_id
    return (Block.view_block view)
