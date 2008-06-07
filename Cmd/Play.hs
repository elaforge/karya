{- | Master control for playing blocks.

    - Find the relevant block to play.
    - Start a thread to start feeding the notelist from the block to its sink.
    - The process is dependent on the backend, and may need access to the midi
    device map and writer (midi backend), or IO (language backend), or socket IO
    (osc backend).
    - Add absolute time to the note events' times.
    - Optionally loop a block.
    - Receive position update msgs and advance the playback selection.

    - On cancel, kill the thread, and invoke a backend specific means to cancel
    outstanding notes (flush midi port, kill external performer, ...).


    Dependent on backend:
    - send notelist to sink
    - convert from absolute time to device specific time
    - for realtime protocols, how far ahead of playback time to stay


    Here's how it works:

    - Deriver generates a performable Score and an inverse tempo map.

    - The Score is passed on to Render, which starts whatever threads are
    necessary to play the Score, and returns a transport control mutable that
    can be used to stop the playback.

    - The transport and tempo map are passed to a play display updater, which
    uses the tempo map to display the play position in the various blocks, and
    aborts along with the performer if the transport says to stop.  It's not
    synchronized to the playback in any way (which may be a separate process
    playing an audio file), but the fact that it's working from the same tempo
    map that generated the audio should keep it in sync.

    - There are three threads involved: the performer is manages whatever
    process needs to perform the score, the updater sweeps the play position
    along, and the app event handler is waiting for events in the responder.

    The performer returns a Transport, which is a mutable variable that holds
    a state value.  The state starts Play, but the performer checks it
    periodically to see if it has become Stop, which the responder can do if
    requested.  This is how the responder sends messages to the performer.  The
    responder uses the presence of the Transport in the Cmd.State to determine
    if there is a performer running or not, and if there is one, how to control
    it.

    The performer sends messages to the responder by writing to the responder
    chan, which is given to it by the responder in the Transport.Info data
    structure when the performer is started.  The responder chan feeds into the
    responder event loop like other msgs.  The performer uses this to report if
    it dies from an error, but Playing and Stopped are actually reported by the
    updater.

    The updater is kicked off simultaneously with the performer, and advances
    the play selection in its own loop, using the tempo map from the deriver.
    It will keep running as long as the transport is at Play and the tempo map
    tells it there is more score to "play".  While the updater doesn't actually
    play anything, it's the one that sends Playing and Stopped msgs to indicate
    performer status.  This is because there may well be multiple simultaneous
    performers that may complete at different times and the updater will only
    emit Stopped if all of them have finished.  If all goes well, the updater
    and the performer will run to completion, the updater will send Stopped,
    and the performer will exit on its own.

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
    eventually run out of TempoMap (which will return Nothing).  The updater
    will send Stopped, which will clear the transport from the responder.  The
    performer is assumed to have completed and exited on its own, probably even
    before the playback audio is completed, since it likely schedules in
    advance.

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
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Sync as Sync

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Schema as Schema

import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Transport as Transport
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Perform as Perform

import qualified App.Config as Config


-- * cmds

cmd_play_block :: Transport.Info -> Cmd.CmdT IO Cmd.Status
cmd_play_block transport_info = do
    cmd_state <- Cmd.get_state
    case Cmd.state_transport cmd_state of
        Just _ -> Log.warn "player already running" >> Cmd.abort
        _ -> return ()
    view_id <- Cmd.get_focused_view
    block_id <- find_play_block view_id
    block <- State.get_block block_id

    (derive_result, tempo_map) <- derive block
    events <- case derive_result of
        -- TODO properly convert to log msg
        Left derive_error -> Log.warn ("derive error: " ++ show derive_error)
            >> Cmd.abort
        Right events -> return events

    -- TODO later, instrument backend dispatches on this
    let (midi_events, convert_warnings) = Convert.convert events
    -- TODO properly convert to log msg
    -- TODO I think this forces the list, I have to not warn so eagerly
    -- or thread the warnings through 'perform'
    -- TODO call Convert.verify for more warnings
    mapM_ (Log.warn . show) convert_warnings
    inst_config <- fmap State.state_midi_config State.get
    let (midi_msgs, perform_warnings) = Perform.perform inst_config midi_events
    mapM_ (Log.warn . show) perform_warnings

    transport <- Trans.liftIO $
        Midi.Play.play transport_info block_id midi_msgs

    ui_state <- State.get
    Trans.liftIO $ Thread.start_thread "play position updater" $
        updater_thread transport transport_info tempo_map ui_state

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
    State.set_play_box block_id (play_state_color status)
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
derive :: (Monad m) => Block.Block ->
    Cmd.CmdT m (Either Derive.DeriveError [Score.Event], Transport.TempoMap)
derive block = do
    deriver <- Schema.get_deriver block
    ui_state <- State.get

    -- TODO all the derivation work could be done asynchronously by
    -- a background thread
    let (result, tempo_map, logs) = Derive.derive ui_state deriver
    mapM_ Log.write logs
    return (result, tempo_map)


-- | Run along the TempoMap and update the play position selection.  Note that
-- this goes directly to the UI through Sync, bypassing the usual state diff
-- folderol.
updater_thread :: Transport.Transport -> Transport.Info
    -> Transport.TempoMap -> State.State -> IO ()
updater_thread transport transport_info tempo_map ui_state = do
    let view_blocks = Map.assocs (Map.map Block.view_block
                (State.state_views ui_state))
        block_ids = map snd view_blocks
        trans_chan = Transport.info_responder_chan transport_info
        get_cur_ts = Transport.info_get_current_timestamp transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    ts_offset <- get_cur_ts
    Exception.bracket_
        (mapM_ (Transport.write_status trans_chan Transport.Playing) block_ids)
        (mapM_ (Transport.write_status trans_chan Transport.Stopped) block_ids)
        (updater_loop transport ts_offset get_cur_ts tempo_map view_blocks)

updater_loop transport ts_offset get_cur_ts tempo_map view_blocks = do
    -- Offset from current time back to block start relative time.
    cur_ts <- fmap (\ts -> ts - ts_offset) get_cur_ts
    let spos = map (\(_, block_id) -> tempo_map block_id cur_ts) view_blocks
    forM_ (zip view_blocks spos) $ \((view_id, _), maybe_pos) ->
        Sync.set_play_position view_id maybe_pos

    tmsg <- Transport.check_transport transport
    -- putStrLn $ "UPDATER at " ++ show cur_ts ++ ": "
    --     ++ show tmsg ++ ", " ++ show spos
    -- Either the transport is stopped or I ran out of tempo map.
    when (tmsg == Transport.Play && not (all Maybe.isNothing spos)) $ do
        Concurrent.threadDelay 30000
        updater_loop transport ts_offset get_cur_ts tempo_map view_blocks


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
