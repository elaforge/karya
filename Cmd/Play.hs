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

    The performer returns a Transport, which is a mutable variable that holds
    a state value.  The state starts at Play, but the performer checks it
    periodically to see if it has become Stop, which the responder can set it
    to if requested.  This is how the responder sends messages to the
    performer.  The responder uses the presence of the Transport in the
    Cmd.State to determine if there is a performer running or not, and if there
    is one, how to control it.

    The performer sends messages to the responder by writing to the responder
    chan, which is given to it by the responder in the 'Transport.Info' data
    structure when the performer is started.  The responder chan feeds into the
    responder event loop like other msgs.  The performer uses this to report if
    it dies from an error, but Playing and Stopped are actually reported by the
    updater.

    The updater is kicked off simultaneously with the performer, and advances
    the play selection in its own loop, using the tempo map from the deriver.
    It will keep running as long as the transport is at Play and the tempo map
    tells it there is more score to \"play\".  While the updater doesn't
    actually play anything, it's the one that sends Playing and Stopped msgs to
    indicate performer status.  This is because there may well be multiple
    simultaneous performers that may complete at different times and the
    updater will only emit Stopped if all of them have finished.  If all goes
    well, the updater and the performer will run to completion, the updater
    will send Stopped, and the performer will exit on its own.

    So in summary, there are two communication channels: the transport, and the
    responder chan.  The transport represents current state and is used to
    communicate from the responder to the performer and the updater, to tell
    them to stop.  The responder chan is used to communicate from the performer
    to the responder (only used to say that the performer died), and from the
    updater to the responder (to tell it when play has started and stopped).
    In addition, the performer will set Stop on the transport if it died.
    Really, only the responder should write to the transport, but a Died will
    definitely trigger a Stop and the performer already has the correct
    Transport, so it's more expedient if it does it directly.

    With multiple backends, there will be multiple transports, and the updater
    will need to monitor them all.

    For example:

    In a normal situation, the performer will do its thing and the updater will
    eventually run out of InverseTempoMap (which will return Nothing).  The
    updater will send Stopped, which will clear the transport from the
    responder.  The performer is assumed to have completed and exited on its
    own, probably even before the playback audio is completed, since it likely
    schedules in advance.

    If the performer dies on an error, it sends a Died to the responder chan.
    As mentioned above, it will also set the transport to Stop.  The updater
    will notice this, and may stop itself, emitting a Stopped msg.  The Stopped
    msg will then cause the responder to clear the transport out of its state,
    which lets it know that play has stopped and it's ok to start another play
    thread.

    If the user requests a stop, the responder sets the transport to Stop.
    Both the performer and updater notice this and quit, the updater emitting
    a Stopped as it does so.  As usual, the Stopped causes the responder to
    clear the transport.
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

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track
-- This causes a bunch of modules to import BlockC.  Can I move the updater
-- stuff out?
import qualified Ui.Sync as Sync

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Schema as Schema

import qualified Perform.Transport as Transport
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Play as Midi.Play
import qualified Instrument.Db as Instrument.Db

import qualified App.Config as Config


-- | Config info needed by the player from the responder.
type PlayInfo = (Instrument.Db.Db, Transport.Info, Schema.SchemaMap)

-- * cmds

cmd_play_focused play_info = do
    view_id <- Cmd.get_focused_view
    block_id <- find_play_block view_id

    -- cmd_play wants to start with a track, so pick the first one.
    block <- State.get_block block_id
    track_id <- Cmd.require $
        Seq.at (Block.track_ids_of (Block.block_tracks block)) 0

    cmd_play play_info block_id (track_id, TrackPos 0)

cmd_play_from_insert play_info = do
    view_id <- Cmd.get_focused_view
    block_id <- find_play_block view_id
    (track_id, _, pos) <- Selection.get_insert_track
    cmd_play play_info block_id (track_id, pos)

cmd_play :: PlayInfo -> Block.BlockId -> (Track.TrackId, TrackPos)
    -> Cmd.CmdT IO Cmd.Status
cmd_play play_info block_id (start_track, start_pos) = do
    let (_, transport_info, schema_map) = play_info
    cmd_state <- Cmd.get_state
    case Cmd.state_transport cmd_state of
        Just _ -> Log.warn "player already running" >> Cmd.abort
        _ -> return ()

    (derive_result, tempo_map, inv_tempo_map) <- derive schema_map block_id
    events <- case derive_result of
        -- TODO properly convert to log msg
        Left derive_error -> Log.warn ("derive error: " ++ show derive_error)
            >> Cmd.abort
        Right events -> return events

    start_ts <- case tempo_map block_id start_track start_pos of
        Nothing -> Cmd.throw $ "unknown play start pos: "
            ++ show start_track ++ ", " ++ show start_pos
        Just ts -> return ts
    transport <- perform block_id play_info start_ts events

    ui_state <- State.get
    Trans.liftIO $ Thread.start_thread "play position updater" $
        updater_thread transport transport_info inv_tempo_map start_ts ui_state

    Cmd.modify_state $ \st -> st { Cmd.state_transport = Just transport }
    return Cmd.Done

cmd_stop :: Cmd.CmdT IO Cmd.Status
cmd_stop = do
    maybe_control <- fmap Cmd.state_transport Cmd.get_state
    transport <- case maybe_control of
        Nothing -> Log.warn "player thread not running" >> Cmd.abort
        Just transport -> return transport
    Trans.liftIO $ Transport.send_transport transport Transport.Stop
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
            st { Cmd.state_transport = Nothing }
        Transport.Died exc -> do
            Log.warn ("player died: " ++ show exc)
            -- Let the updater know this performer has stopped.
            cmd_stop
            return ()
    return Cmd.Done

-- * implementation

-- | Derive the contents of the given block to score events.
derive :: (Monad m) => Schema.SchemaMap -> Block.BlockId -> Cmd.CmdT m
    (Either Derive.DeriveError [Score.Event], Transport.TempoFunction,
        Transport.InverseTempoFunction)
derive schema_map block_id = do
    ui_state <- State.get
    let (result, tempo_func, inv_tempo_func, logs, _) =
            Derive.derive (Schema.lookup_deriver schema_map ui_state)
                ui_state False (Derive.d_block block_id)
    mapM_ Log.write logs
    return (result, tempo_func, inv_tempo_func)

perform :: Block.BlockId -> PlayInfo -> Timestamp.Timestamp -> [Score.Event]
    -> Cmd.CmdT IO Transport.Transport
perform block_id (inst_db, transport_info, _) start_ts events = do
    let lookup_inst = Instrument.Db.db_lookup_midi inst_db
    let (midi_events, convert_warnings) = Convert.convert
            lookup_inst (seek_events (Timestamp.to_track_pos start_ts) events)

    -- TODO split events up by backend and dispatch to each backend
    -- TODO properly convert to log msg
    -- TODO I think this forces the list, I have to not warn so eagerly
    -- or thread the warnings through 'perform'
    -- TODO call Convert.verify for more warnings
    mapM_ (Log.warn . show) convert_warnings
    inst_config <- fmap State.state_midi_config State.get
    let (midi_msgs, perform_warnings) =
            Perform.perform lookup_inst inst_config midi_events
    mapM_ (Log.warn . show) perform_warnings

    -- block_id is only used for log msgs.
    Trans.liftIO $ Midi.Play.play transport_info block_id start_ts midi_msgs

seek_events :: TrackPos -> [Score.Event] -> [Score.Event]
seek_events pos events = dropWhile ((< pos) . Score.event_start) events


-- | Run along the InverseTempoMap and update the play position selection.
-- Note that this goes directly to the UI through Sync, bypassing the usual
-- state diff folderol.
updater_thread :: Transport.Transport -> Transport.Info
    -> Transport.InverseTempoFunction -> Timestamp.Timestamp -> State.State
    -> IO ()
updater_thread transport transport_info inv_tempo_func start_ts ui_state = do
    -- Send Playing and Stopped msgs to the responder for all visible blocks.
    let block_ids = Seq.unique $ Map.elems (Map.map Block.view_block
            (State.state_views ui_state))
        trans_chan = Transport.info_responder_chan transport_info
        get_cur_ts = Transport.info_get_current_timestamp transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    ts_offset <- get_cur_ts
    let state = UpdaterState transport (ts_offset - start_ts) get_cur_ts
            inv_tempo_func Set.empty ui_state
    Exception.bracket_
        (mapM_ (Transport.write_status trans_chan Transport.Playing) block_ids)
        (mapM_ (Transport.write_status trans_chan Transport.Stopped) block_ids)
        (updater_loop state)

data UpdaterState = UpdaterState {
    updater_transport :: Transport.Transport
    , updater_ts_offset :: Timestamp.Timestamp
    , updater_get_cur_ts :: IO Timestamp.Timestamp
    , updater_inv_tempo_func :: Transport.InverseTempoFunction
    , updater_active_sels :: Set.Set (Block.ViewId, [Block.TrackNum])
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

    tmsg <- Transport.check_transport (updater_transport state)
    -- putStrLn $ "UPDATER at " ++ show cur_ts ++ ": "
    -- pprint play_pos
    -- ++ show tmsg ++ ", " ++ show block_pos ++ ", gone: " ++ show gone
    -- putStrLn updater_status
    if tmsg /= Transport.Play || null block_pos
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
    [(Block.BlockId, [(Track.TrackId, TrackPos)])]
    -> m [(Block.ViewId, [(Block.TrackNum, Maybe TrackPos)])]
block_pos_to_play_pos block_pos = fmap concat (mapM convert block_pos)

convert :: (State.UiStateMonad m) =>
    (Block.BlockId, [(Track.TrackId, TrackPos)])
    -> m [(Block.ViewId, [(Block.TrackNum, Maybe TrackPos)])]
convert (block_id, track_pos) = do
    view_ids <- fmap Map.keys (State.get_views_of block_id)
    block <- State.get_block block_id
    let tracknum_pos = concatMap (tracknums_of block) track_pos
    return [(view_id, tracknum_pos) | view_id <- view_ids]

tracknums_of :: Block.Block -> (Track.TrackId, TrackPos)
    -> [(Block.TrackNum, Maybe TrackPos)]
tracknums_of block (track_id, pos) =
    [ (tracknum, Just pos)
    | (tracknum, Block.TId tid _) <- zip [0..] (Block.block_tracks block)
    , tid == track_id ]


-- * util

play_state_color status = case status of
    Transport.Playing -> Config.play_color
    Transport.Died _ -> Config.warning_color
    _ -> Config.box_color

-- | Find the block to play, relative to the given view.
-- find_play_block :: State.State -> Block.ViewId -> Block.BlockId
find_play_block view_id = do
    view <- State.get_view view_id
    return (Block.view_block view)
