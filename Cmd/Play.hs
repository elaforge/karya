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

    - There are three threads involved: the performer is manages whatever
    process needs to perform the score, the updater sweeps the play position
    along, and the app event handler is waiting for events in the responder.

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
    'cmd_transport_msg', which can use them to set UI state like changing the
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
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
-- This causes a bunch of modules to import BlockC.  Can I move the updater
-- stuff out?
import qualified Ui.Sync as Sync
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Schema as Schema

import qualified Perform.Transport as Transport
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Play as Midi.Play
import qualified Instrument.Db as Instrument.Db

import qualified App.Config as Config


-- * cmds

cmd_play_focused :: Transport.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_focused transport_info = do
    block_id <- Cmd.get_focused_block
    -- cmd_play wants to start with a track, so pick the first one.
    -- TODO: pick the first event track!
    block <- State.get_block block_id
    track_id <- Cmd.require $ Seq.at (Block.block_track_ids block) 0
    cmd_play transport_info block_id (track_id, TrackPos 0)

cmd_play_from_insert :: Transport.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_from_insert transport_info = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    cmd_play transport_info block_id (track_id, pos)

cmd_play :: Transport.Info -> BlockId -> (TrackId, TrackPos)
    -> Cmd.CmdT IO Cmd.Status
cmd_play transport_info block_id (start_track, start_pos) = do
    cmd_state <- Cmd.get_state
    case Cmd.state_play_control cmd_state of
        Just _ -> Cmd.throw "player already running"
        _ -> return ()
    perf <- get_performance block_id

    start_ts <- case Cmd.perf_tempo perf block_id start_track start_pos of
        Nothing -> Cmd.throw $ "unknown play start pos: "
            ++ show start_track ++ ", " ++ show start_pos
        Just ts -> return ts
    let msgs = seek_msgs start_ts (Cmd.perf_msgs perf)
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

-- | Respond to transport status msgs coming back from the player thread.
cmd_transport_msg :: Msg.Msg -> Cmd.CmdT IO Cmd.Status
cmd_transport_msg msg = do
    (block_id, status) <- case msg of
        Msg.Transport (Transport.Status block_id status) ->
            return (block_id, status)
        _ -> Cmd.abort
    block_ids <- fmap (Map.keys . State.state_blocks) State.get
    mapM_ (flip State.set_play_box (play_state_color status)) block_ids

    Log.notice $ "player status for " ++ show block_id ++ ": " ++ show status
    case status of
        Transport.Playing -> return ()
        -- Either the performer has declared itself stopped, or the updater
        -- has declared it stopped.  In any case, I don't need a transport
        -- to tell it what to do anymore.
        Transport.Stopped -> Cmd.modify_state $ \st ->
            st { Cmd.state_play_control = Nothing }
        Transport.Died err_msg -> Log.warn ("player died: " ++ err_msg)
    return Cmd.Done

-- * implementation

seek_msgs :: Timestamp.Timestamp -> [Midi.WriteMessage] -> [Midi.WriteMessage]
seek_msgs start_ts midi_msgs = map (Midi.add_timestamp (-start_ts)) $
    dropWhile ((<start_ts) . Midi.wmsg_ts) midi_msgs
    -- TODO This would be inefficient starting in the middle of a big block,
    -- but it's simple and maybe fast enough.  Otherwise, maybe I put the msgs
    -- in an array and bsearch?  Or a list of chunks?

get_performance :: (Monad m) => BlockId -> Cmd.CmdT m Cmd.Performance
get_performance block_id = do
    by_block <- fmap Cmd.state_performance Cmd.get_state
    maybe (State.throw $ "no performance for block " ++ show block_id) return
        (Map.lookup block_id by_block)

-- ** perform

-- | Convert a block ID into MIDI msgs and log msgs.  The logs are not
-- immediately written to preserve laziness on the returned MIDI msgs.
-- This is actually called from ResponderSync, when it kicks off background
-- derivation.  By the time 'cmd_play' pulls out the Performance, it should be
-- at least partially evaluated.
perform :: (Monad m) => BlockId -> Instrument.Db.Db -> Schema.SchemaMap
    -> Cmd.CmdT m Cmd.Performance
perform block_id inst_db schema_map = do
    (derive_result, tempo, inv_tempo) <- derive schema_map block_id
    events <- case derive_result of
        -- TODO properly convert to log msg
        Left derive_error -> Log.warn ("derive error: " ++ show derive_error)
            >> Cmd.abort
        Right events -> return events

    let lookup_inst = Instrument.Db.db_lookup_midi inst_db
    let (midi_events, convert_warnings) = Convert.convert lookup_inst events

    -- TODO call Convert.verify for more warnings
    inst_config <- fmap State.state_midi_config State.get
    let (midi_msgs, perform_warnings) =
            Perform.perform lookup_inst inst_config midi_events
    let logs = map (warn_to_msg "event conversion") convert_warnings
            ++ map (warn_to_msg "performance") perform_warnings
    return $ Cmd.Performance midi_msgs logs tempo inv_tempo

-- | Derive the contents of the given block to score events.
derive :: (Monad m) => Schema.SchemaMap -> BlockId -> Cmd.CmdT m
    (Either Derive.DeriveError [Score.Event], Transport.TempoFunction,
        Transport.InverseTempoFunction)
derive schema_map block_id = do
    ui_state <- State.get
    let (result, tempo_func, inv_tempo_func, logs, _) =
            Derive.derive (Schema.lookup_deriver schema_map ui_state)
                ui_state False (Derive.d_block block_id)
    -- TODO does this force the derivation?
    mapM_ Log.write logs
    return (result, tempo_func, inv_tempo_func)

-- | Convert a Warning into an appropriate log msg.
warn_to_msg :: String -> Warning.Warning -> Log.Msg
warn_to_msg context (Warning.Warning msg event_stack maybe_range) =
    log { Log.msg_stack = Just event_stack }
    where
    log = Log.msg Log.Warn $ context ++ ": " ++ msg
        -- TODO It would be more useful to append this to the stack, but I have
        -- to convert global -> local.
        ++ maybe "" (("pos: " ++) . show) maybe_range


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
        trans_chan = Transport.info_responder_chan transport_info
        get_cur_ts = Transport.info_get_current_timestamp transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    ts_offset <- get_cur_ts
    let state = UpdaterState ctl (ts_offset - start_ts) get_cur_ts
            inv_tempo_func Set.empty ui_state
    Exception.bracket_
        (mapM_ (Transport.write_status trans_chan Transport.Playing) block_ids)
        (mapM_ (Transport.write_status trans_chan Transport.Stopped) block_ids)
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
    cur_ts <- fmap (+ (- updater_ts_offset state)) (updater_get_cur_ts state)

    let block_pos = updater_inv_tempo_func state cur_ts
    play_pos <- either
        (\err -> Log.error ("state error in updater: " ++ show err)
            >> return [])
        return
        (State.eval (updater_ui_state state)
            $ block_pos_to_play_pos block_pos)
    Sync.set_play_position play_pos

    let active_sels = Set.fromList
            [(view_id, map fst num_pos) | (view_id, num_pos) <- play_pos]
    clear_play_position (Set.difference (updater_active_sels state) active_sels)
    state <- return $ state { updater_active_sels = active_sels }

    stopped <- Transport.check_player_stopped (updater_ctl state)
    -- putStrLn $ "UPDATER at " ++ show cur_ts ++ ": "
    -- pprint play_pos
    -- ++ show tmsg ++ ", " ++ show block_pos ++ ", gone: " ++ show gone
    -- putStrLn updater_status
    if stopped || null block_pos
        then clear_play_position (updater_active_sels state)
        else do
            Concurrent.threadDelay 40000
            updater_loop state
    where
    clear_play_position view_nums = Sync.set_play_position
        [ (view_id, map (flip (,) Nothing) nums)
        | (view_id, nums) <- Set.toList view_nums ]


-- | Do all the annoying shuffling around to convert the deriver-oriented
-- blocks and tracks to the view-oriented views and tracknums.
block_pos_to_play_pos :: (State.UiStateMonad m) =>
    [(BlockId, [(TrackId, TrackPos)])]
    -> m [(ViewId, [(TrackNum, Maybe TrackPos)])]
block_pos_to_play_pos block_pos = fmap concat (mapM convert block_pos)

convert :: (State.UiStateMonad m) =>
    (BlockId, [(TrackId, TrackPos)])
    -> m [(ViewId, [(TrackNum, Maybe TrackPos)])]
convert (block_id, track_pos) = do
    view_ids <- fmap Map.keys (State.get_views_of block_id)
    block <- State.get_block block_id
    let tracknum_pos = concatMap (tracknums_of block) track_pos
    return [(view_id, tracknum_pos) | view_id <- view_ids]

tracknums_of :: Block.Block -> (TrackId, TrackPos)
    -> [(TrackNum, Maybe TrackPos)]
tracknums_of block (track_id, pos) =
    [ (tracknum, Just pos)
    | (tracknum, Block.TId tid _) <- zip [0..] (Block.block_tracklike_ids block)
    , tid == track_id ]


-- * util

play_state_color status = case status of
    Transport.Playing -> Config.play_color
    Transport.Died _ -> Config.warning_color
    _ -> Config.box_color

-- | Find the block to play, relative to the given view.
-- find_play_block :: State.State -> ViewId -> BlockId
find_play_block view_id = do
    view <- State.get_view view_id
    return (Block.view_block view)
