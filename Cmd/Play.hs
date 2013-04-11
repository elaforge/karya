{- | Master control for playing blocks.

    Here's how it works:

    - Find the relevant block to play.

    - Deriver generates a performable Score and an inverse tempo map.

    - The Score is preprocessed by adding the current absolute time to it
    and skipping notes based on the start offset.

    - The Score is sent to the Performer, which splits it up by backend,
    starts whatever processes are necessary to play the notes, and returns
    a transport control mutable that can be used to stop the playback.

    - The transport and tempo map are passed to a play monitor thread, which
    uses the tempo map to display the play position in the various blocks, and
    aborts along with the performer if the transport says to stop.  It's not
    synchronized to the playback in any way (which may be a separate process
    playing an audio file), but the fact that it's working from the same tempo
    map that generated the audio should keep it in sync.

    - There are three threads involved: the performer manages whatever process
    needs to perform the score, the play monitor sweeps the play position
    along, and the app event handler is waiting for events in the responder.

    - On cancel, kill the thread, and invoke a backend specific means to cancel
    outstanding notes (flush midi port, kill external performer, ...).

    The player returns controls to communicate with the player and the play
    monitor.  If the responder sets the player control, the player will quit.
    The player stopping causes it to set the 'Transport.PlayMonitorControl',
    which causes the monitor to quit (if there are multiple players, the
    monitor should wait for them all to quit).

    There's a third control, which is a channel given to the player by the
    responder.  Both the player and the monitor use it to send transport msgs
    to the responder.  All the player sends is a Died msg which can be logged
    when the player as started and stopped.  Transport msgs wind up in
    'cmd_play_msg', which can use them to set UI state like changing the
    play box color and logging.

    The play monitor is kicked off simultaneously with the performer, and
    advances the play selection in its own loop, using the tempo map from the
    deriver.  It will keep running until it gets a stop msg from the control or
    the tempo map tells it there is no more score to \"play\".  While the
    monitor doesn't actually play anything, it's the one that sends Playing and
    Stopped transport msgs to indicate performer status.  This is because there
    may be multiple simultaneous performers that may complete at different
    times and the monitor will only emit Stopped if all of them have finished.
    If all goes well, the monitor and the performer will run to completion, the
    monitor will send Stopped, and the performer will exit on its own.

    With multiple backends, there will be multiple monitor controls, and the
    monitor will need to monitor them all.

    For example:

    In a normal situation, the performer will do its thing and the monitor will
    eventually run out of InverseTempoMap (which will return Nothing).  The
    monitor will send Stopped, which will clear the player control from the
    responder Cmd.State, which is how the UI knows whether playing is in
    progress.  The performer is assumed to have completed and exited on its
    own, probably even before the playback audio is completed, since it likely
    schedules in advance.

    If the performer dies on an error, it sends a Died to the responder chan.
    As mentioned above, it will also tell the monitor to stop.  The monitor
    will notice this, and may stop itself, emitting a Stopped msg.  The Stopped
    msg will then cause the responder to clear the player control out of its
    state, which lets it know that play has stopped and it's ok to start
    another play thread.

    If the user requests a stop, the responder sets the player control to Stop.
    The player stops, telling the monitor to stop, which emits Stopped, which
    clears the PlayMonitorControl.
-}
module Cmd.Play where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.LEvent as LEvent
import qualified Perform.Transport as Transport
import Types


-- * stop

-- | Context sensitive stop that stops whatever is going on.  First it stops
-- realtime play, then step play, and then it just sends all notes off.
cmd_context_stop :: Cmd.CmdIO
cmd_context_stop = do
    maybe_ctl <- gets Cmd.state_play_control
    if_just maybe_ctl (done . liftIO . Transport.stop_player) $ do
    step_playing <- Cmd.gets (Maybe.isJust . Cmd.state_step . Cmd.state_play)
    if step_playing then done StepPlay.cmd_clear
        else done Cmd.all_notes_off
    where
    done = (>> return Cmd.Done)

cmd_stop :: Cmd.CmdIO
cmd_stop = do
    maybe_ctl <- gets Cmd.state_play_control
    when_just maybe_ctl (void . liftIO . Transport.stop_player)
    return Cmd.Done

-- * play

local_block :: (Cmd.M m) => m Cmd.PlayMidiArgs
local_block = do
    block_id <- Cmd.get_focused_block
    from_score block_id Nothing 0 Nothing

-- | Start playing from the point selection on the local block.  If the
-- selection is a range, loop that range forever.
local_selection :: (Cmd.M m) => m Cmd.PlayMidiArgs
local_selection = do
    (block_id, _, track_id, _) <- Selection.get_insert
    (_, sel) <- Selection.get
    let (pos, repeat_at) = if Types.sel_is_point sel
            then (Types.sel_start_pos sel, Nothing)
            else Just <$> Types.sel_range sel
    from_score block_id (Just track_id) pos repeat_at

local_previous :: (Cmd.M m) => m Cmd.PlayMidiArgs
local_previous = do
    step <- gets Cmd.state_play_step
    (block_id, tracknum, track_id, pos) <- Selection.get_insert
    prev <- TimeStep.rewind step block_id tracknum pos
    from_score block_id (Just track_id) (fromMaybe 0 prev) Nothing

-- | Play the root block from the beginning.
root_block :: (Cmd.M m) => m Cmd.PlayMidiArgs
root_block = do
    block_id <- State.get_root_id
    from_score block_id Nothing 0 Nothing

-- | Play the root performance from the selection on the root block.  This
-- is useful to manually set a point to start playing.
root_from_root_selection :: (Cmd.M m) => m Cmd.PlayMidiArgs
root_from_root_selection = do
    (block_id, _, track_id, pos) <- Selection.get_root_insert
    from_score block_id (Just track_id) pos Nothing

-- | Play the root performance from the selection on the current block.  If
-- this block isn't linked from the root, then fall back on 'local_selection'.
root_from_local_selection :: (Cmd.M m) => m Cmd.PlayMidiArgs
root_from_local_selection = do
    (block_id, _, track_id, pos) <- Selection.get_insert
    root_id <- State.get_root_id
    perf <- get_performance root_id
    maybe local_selection (from_realtime root_id Nothing)
        =<< Perf.lookup_realtime perf block_id (Just track_id) pos

-- | Find the previous step on the focused block, get its RealTime, and play
-- from the root at that RealTime.  If this block isn't linked from the root,
-- then fall back on 'local_previous'.
root_previous :: (Cmd.M m) => m Cmd.PlayMidiArgs
root_previous = do
    (block_id, tracknum, track_id, pos) <- Selection.get_insert
    step <- gets Cmd.state_play_step
    prev <- fromMaybe pos <$> TimeStep.rewind step block_id tracknum pos
    root_id <- State.get_root_id
    perf <- get_performance root_id
    maybe local_previous (from_realtime root_id Nothing)
        =<< Perf.lookup_realtime perf block_id (Just track_id) prev

from_score :: (Cmd.M m) => BlockId
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

get_realtime :: (Cmd.M m) => BlockId
    -- ^ Lookup realtime according to the performance of this block.
    -> BlockId
    -- ^ Lookup realtime at the position (TrackId, ScoreTime) within this block.
    -> Maybe TrackId -> ScoreTime
    -> m RealTime
get_realtime perf_block play_block maybe_track_id pos = do
    perf <- get_performance perf_block
    maybe_start <- Perf.lookup_realtime perf play_block maybe_track_id pos
    case maybe_start of
        Nothing -> do
            -- Otherwise we don't get to see why it failed.
            mapM_ Log.write $ LEvent.logs_of (Cmd.perf_events perf)
            Cmd.throw $ "play " ++ show perf_block
                ++ " has no tempo information"
        Just start -> return start

get_performance :: (Cmd.M m) => BlockId -> m Cmd.Performance
get_performance block_id =
    Cmd.require_msg ("no performance for block " ++ show block_id)
        =<< lookup_current_performance block_id

-- | Play the performance of the given block starting from the given time.
from_realtime :: (Cmd.M m) => BlockId -> Maybe RealTime -> RealTime
    -> m Cmd.PlayMidiArgs
from_realtime block_id repeat_at start = do
    play_control <- gets Cmd.state_play_control
    when_just play_control $ \_ -> Cmd.throw "player already running"

    perf <- Cmd.require_msg ("no performance for block " ++ show block_id)
        =<< lookup_current_performance block_id
    multiplier <- gets (recip . Cmd.state_play_multiplier)

    -- Events can wind up before 0, say if there's a grace note on a note at 0.
    -- To have them play correctly, perform_from will give me negative events
    -- when starting from 0, and then I have to shift the start time back to
    -- consider the first event the new 0.
    msgs <- PlayUtil.perform_from start perf
    start <- let mstart = PlayUtil.first_time msgs
        in return $ if start == 0 && mstart < 0 then mstart else start
    msgs <- return $ PlayUtil.shift_messages multiplier start msgs
    -- See doc for "Cmd.PlayC".
    return $ Cmd.PlayMidiArgs (Pretty.pretty block_id) msgs
        (Just (Cmd.perf_inv_tempo perf . (+start) . (/multiplier)))
        ((*multiplier) . subtract start <$> repeat_at)

lookup_current_performance :: (Cmd.M m) => BlockId -> m (Maybe Cmd.Performance)
lookup_current_performance block_id = Map.lookup block_id <$>
    gets Cmd.state_current_performance

-- * implementation

gets :: (Cmd.M m) => (Cmd.PlayState -> a) -> m a
gets f = Cmd.gets (f . Cmd.state_play)
