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
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
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
import qualified Cmd.Perf as Perf
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Transport as Transport
import qualified App.Config as Config


-- * cmds

cmd_play_focused :: Transport.Info -> Cmd.CmdIO
cmd_play_focused transport_info = do
    block_id <- Cmd.get_focused_block
    cmd_play transport_info block_id (Nothing, 0)

cmd_play_from_insert :: Transport.Info -> Cmd.CmdIO
cmd_play_from_insert transport_info = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    cmd_play transport_info block_id (Just track_id, pos)

cmd_play_from_previous_step :: Transport.Info -> Cmd.CmdIO
cmd_play_from_previous_step transport_info = do
    step <- Cmd.state_play_step <$> get
    (block_id, tracknum, track_id, pos) <- Selection.get_insert
    prev <- TimeStep.rewind step block_id tracknum pos
    cmd_play transport_info block_id (Just track_id, maybe 0 id prev)

cmd_play_from_previous_root_step :: Transport.Info -> Cmd.CmdIO
cmd_play_from_previous_root_step transport_info = do
    (block_id, tracknum, track_id, pos) <- Selection.get_root_insert
    step <- Cmd.state_play_step <$> get
    prev <- TimeStep.rewind step block_id tracknum pos
    cmd_play transport_info block_id (Just track_id, maybe 0 id prev)

cmd_play :: Transport.Info -> BlockId -> (Maybe TrackId, ScoreTime)
    -> Cmd.CmdIO
cmd_play transport_info block_id (start_track, start_pos) = do
    state <- get
    case Cmd.state_play_control state of
        Just _ -> Cmd.throw "player already running"
        _ -> return ()
    perf <- Cmd.require_msg ("no performance for block " ++ show block_id)
        =<< Cmd.lookup_performance block_id
    start <- Perf.get_realtime perf block_id start_track start_pos
    msgs <- PlayUtil.shift_messages start <$> PlayUtil.perform_from start perf
    (play_ctl, updater_ctl) <- Trans.liftIO $
        Midi.Play.play transport_info block_id msgs

    ui_state <- State.get
    Trans.liftIO $ do
        -- Pass the current state in the MVar.  ResponderSync will keep it up
        -- to date afterwards, but only if blocks are added or removed.
        MVar.modifyMVar_ (Transport.info_state transport_info)
            (const (return ui_state))
        Thread.start $ updater_thread
            updater_ctl transport_info (Cmd.perf_inv_tempo perf) start
    modify $ \st -> st { Cmd.state_play_control = Just play_ctl }
    return Cmd.Done

-- | Context sensitive stop that stops whatever is going on.  First it stops
-- realtime play, then step play, and then it just sends all notes off.
cmd_context_stop :: Cmd.CmdIO
cmd_context_stop = do
    maybe_ctl <- Cmd.state_play_control <$> get
    if_just maybe_ctl (done . Trans.liftIO . Transport.stop_player) $ do
    step_playing <- Cmd.gets (Maybe.isJust . Cmd.state_step . Cmd.state_play)
    if step_playing then (done StepPlay.cmd_clear) else do
    done Cmd.all_notes_off
    where
    done = (>> return Cmd.Done)

cmd_stop :: Cmd.CmdIO
cmd_stop = do
    maybe_ctl <- Cmd.state_play_control <$> get
    when_just maybe_ctl (void . Trans.liftIO . Transport.stop_player)
    return Cmd.Done

-- | Respond to msgs about derivation and playing status.
cmd_play_msg :: Msg.Msg -> Cmd.CmdIO
cmd_play_msg msg = do
    case msg of
        Msg.Transport status -> transport_msg status
        Msg.DeriveStatus block_id status -> derive_status_msg block_id status
        _ -> Cmd.abort
    return Cmd.Done
    where
    transport_msg status = case status of
        Transport.Playing -> return ()
        -- Either the performer has declared itself stopped, or the updater
        -- has declared it stopped.  In any case, I don't need a transport
        -- to tell it what to do anymore.
        Transport.Stopped ->
            modify $ \st -> st { Cmd.state_play_control = Nothing }
        Transport.Died err_msg -> Log.warn ("player died: " ++ err_msg)
    derive_status_msg block_id status = do
        State.set_play_box block_id (derive_status_color status)
        case status of
            Msg.DeriveComplete perf -> do
                Cmd.modify_play_state $ \st ->
                    st { Cmd.state_performance = Map.insert block_id
                         perf (Cmd.state_performance st) }
                ui_state <- State.get
                Trans.liftIO $ Sync.set_track_signals ui_state
                    (Cmd.perf_track_signals perf)
            _ -> return ()
    derive_status_color status = case status of
        Msg.OutOfDate -> Color.brightness 1.5 Config.busy_color
        Msg.Deriving -> Config.busy_color
        Msg.DeriveComplete _ -> Config.box_color

-- * implementation

get :: (Cmd.M m) => m Cmd.PlayState
get = Cmd.gets Cmd.state_play

modify :: (Cmd.M m) => (Cmd.PlayState -> Cmd.PlayState) -> m ()
modify = Cmd.modify_play_state

-- ** updater

-- | Run along the InverseTempoMap and update the play position selection.
-- Note that this goes directly to the UI through Sync, bypassing the usual
-- state diff folderol.
updater_thread :: Transport.UpdaterControl -> Transport.Info
    -> Transport.InverseTempoFunction -> RealTime -> IO ()
updater_thread ctl transport_info inv_tempo_func start = do
    let get_now = Transport.info_get_current_time transport_info
    -- This won't be exactly the same as the renderer's ts offset, but it's
    -- probably close enough.
    offset <- get_now
    let state = UpdaterState ctl (offset - start) get_now
            inv_tempo_func Set.empty (Transport.info_state transport_info)
    let send status = Transport.info_send_status transport_info status
    Exception.bracket_ (send Transport.Playing) (send Transport.Stopped)
        (updater_loop state)

data UpdaterState = UpdaterState {
    updater_ctl :: Transport.UpdaterControl
    , updater_offset :: RealTime
    , updater_get_now :: IO RealTime
    , updater_inv_tempo_func :: Transport.InverseTempoFunction
    , updater_active_sels :: Set.Set (ViewId, [TrackNum])
    , updater_ui_state :: MVar.MVar State.State
    }

updater_loop :: UpdaterState -> IO ()
updater_loop state = do
    now <- subtract (updater_offset state) <$> updater_get_now state
    let fail err = Log.error ("state error in updater: " ++ show err)
            >> return []
    ui_state <- MVar.readMVar (updater_ui_state state)
    play_pos <- either fail return $ State.eval ui_state $
        Perf.find_play_pos (updater_inv_tempo_func state) now
    Sync.set_play_position play_pos

    let active_sels = Set.fromList
            [(view_id, map fst num_pos) | (view_id, num_pos) <- play_pos]
    mapM_ (Sync.clear_play_position . fst) $
        Set.toList (Set.difference (updater_active_sels state) active_sels)
    state <- return $ state { updater_active_sels = active_sels }

    stopped <- Transport.check_player_stopped (updater_ctl state)
    if stopped || null (updater_inv_tempo_func state now)
        then mapM_ (Sync.clear_play_position . fst) $
            Set.toList (updater_active_sels state)
        else Thread.delay 0.05 >> updater_loop state


-- * util

-- | Find the block to play, relative to the given view.
-- find_play_block :: State.State -> ViewId -> BlockId
find_play_block view_id = do
    view <- State.get_view view_id
    return (Block.view_block view)
