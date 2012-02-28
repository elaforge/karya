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
import Control.Monad
import qualified Control.Monad.Trans as Trans

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Midi.Play as Midi.Play
import qualified Perform.Transport as Transport
import Types


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
    step <- gets Cmd.state_play_step
    (block_id, tracknum, track_id, pos) <- Selection.get_insert
    prev <- TimeStep.rewind step block_id tracknum pos
    cmd_play transport_info block_id (Just track_id, maybe 0 id prev)

cmd_play_from_previous_root_step :: Transport.Info -> Cmd.CmdIO
cmd_play_from_previous_root_step transport_info = do
    (block_id, tracknum, track_id, pos) <- Selection.get_root_insert
    step <- gets Cmd.state_play_step
    prev <- TimeStep.rewind step block_id tracknum pos
    cmd_play transport_info block_id (Just track_id, maybe 0 id prev)

cmd_play :: Transport.Info -> BlockId -> (Maybe TrackId, ScoreTime)
    -> Cmd.CmdIO
cmd_play transport_info block_id (start_track, start_pos) = do
    state <- gets id
    case Cmd.state_play_control state of
        Just _ -> Cmd.throw "player already running"
        _ -> return ()

    perf <- Cmd.require_msg ("no performance for block " ++ show block_id)
        =<< lookup_current_performance block_id
    start <- Perf.get_realtime perf block_id start_track start_pos
    msgs <- PlayUtil.shift_messages start <$> PlayUtil.perform_from start perf
    Log.debug $ "play block " ++ show block_id
    (play_ctl, updater_ctl) <- Trans.liftIO $
        Midi.Play.play transport_info block_id msgs

    ui_state <- State.get
    -- Pass the current state in the MVar.  ResponderSync will keep it up
    -- to date afterwards, but only if blocks are added or removed.
    Trans.liftIO $ MVar.modifyMVar_ (Transport.info_state transport_info)
        (const (return ui_state))
    Cmd.modify_play_state $ \st -> st { Cmd.state_play_control = Just play_ctl }
    -- See doc for "Cmd.PlayC".
    return $ Cmd.Play $
        Cmd.UpdaterArgs updater_ctl (Cmd.perf_inv_tempo perf) start

lookup_current_performance :: (Cmd.M m) => BlockId -> m (Maybe Cmd.Performance)
lookup_current_performance block_id = Map.lookup block_id <$>
    gets Cmd.state_current_performance

-- | Context sensitive stop that stops whatever is going on.  First it stops
-- realtime play, then step play, and then it just sends all notes off.
cmd_context_stop :: Cmd.CmdIO
cmd_context_stop = do
    maybe_ctl <- gets Cmd.state_play_control
    if_just maybe_ctl (done . Trans.liftIO . Transport.stop_player) $ do
    step_playing <- Cmd.gets (Maybe.isJust . Cmd.state_step . Cmd.state_play)
    if step_playing then (done StepPlay.cmd_clear) else do
    done Cmd.all_notes_off
    where
    done = (>> return Cmd.Done)

cmd_stop :: Cmd.CmdIO
cmd_stop = do
    maybe_ctl <- gets Cmd.state_play_control
    when_just maybe_ctl (void . Trans.liftIO . Transport.stop_player)
    return Cmd.Done

-- * implementation

gets :: (Cmd.M m) => (Cmd.PlayState -> a) -> m a
gets f = Cmd.gets (f . Cmd.state_play)
