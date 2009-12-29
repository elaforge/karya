{- | Subset of the responder that handles syncing from State.State to the UI.
    This is also where the signal display mechanism lives.

    The signal display mechanism goes from the deriver to the UI, so it's
    spread across Schema, Derive, Responder, to Sync.  Effectively this is
    a path from the deriver to the UI, just as the "play" path is from the
    deriver to Perform.  Schema has a special "signal_deriver" field, and the
    signal deriver is compiled from the Skeleton in a way parallel the score
    event deriver.

    While the event deriver is invoked at play time, the signal deriver is
    invoked by the responder before syncing the state to the UI.  The deriver
    creates signal maps for each track for each block.  It's important that the
    signal maps remain lazy because only portions of them may be needed if
    there are only localized track updates (or none at all if there are no
    track updates).

    A track's samples are much like its events in that they are spread across
    the track's entire range but only displayed for the visible subset.
    However, while events have a special mechanism/hack to capture modified
    ranges (as described in Ui.State) so the display can be refreshed
    incrementally, samples have no such mechanism.  This is because they are
    expected to only change in reaction to events changing, and to only change
    within the range of the changed events.  Since an event alteration will
    emit Update.TrackEvents, the track should be redrawn in the given range and
    refresh the altered sample area.

    TODO I'm not entirely satisfied with this, elegance-wise or
    efficiency-wise.  I'll wait until I have experience with large tracks and
    dense samples before giving this more thought.  It seems like I should be
    able to render samples directly to a buffer which gets passed by pointer to
    c++.
-}
module Cmd.ResponderSync where
import Control.Monad
import qualified Control.Arrow as Arrow
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Map as Map
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import Ui
import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play

import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema

import qualified Perform.Signal as Signal


-- | Sync @state2@ to the UI.
-- Returns both UI state and cmd state since verification may clean up the UI
-- state, and this is where the undo history is stored in Cmd.State.
sync :: State.State -> State.State -> Cmd.State
    -> [Update.Update] -> IO ([Update.Update], State.State, Cmd.State)
sync ui_from ui_to cmd_state cmd_updates = do
    -- I'd catch problems closer to their source if I did this from run_cmds,
    -- but it's nice to see that it's definitely happening before syncs.
    ui_to <- verify_state ui_to

    let (block_samples, sig_logs) = derive_signals
            (Cmd.state_schema_map cmd_state) ui_to
    -- Don't want to force block_samples because it computes a lot of stuff
    -- that sync may never need.  TODO how can I lazily write log msgs?
    -- Actually, the expensive part is the sampling, which has no logging,
    -- so it should remain lazy... I think.  Verify this.
    mapM_ Log.write sig_logs
    Log.timer "wrote signal logs"
    -- putStrLn $ "block samples: " ++ show block_samples
    diff_updates <- case Diff.diff ui_from ui_to of
        Left err -> Log.error ("diff error: " ++ err) >> return []
        Right diff_updates -> do
            unless ((null diff_updates) && (null cmd_updates)) $
                Log.debug $ "diff_updates: " ++ show diff_updates
                    ++ " cmd_updates: " ++ show cmd_updates
            Log.timer "got diff updates"
            err <- Sync.sync ui_to (diff_updates ++ cmd_updates) block_samples
            case err of
                Nothing -> return ()
                Just err -> Log.error $ "syncing updates: " ++ show err
            return diff_updates

    let updates = diff_updates ++ cmd_updates
    -- Kick off the background derivation threads.
    cmd_state <- run_derive ui_from ui_to cmd_state updates
    return (updates, ui_to, cmd_state)

-- | This should be run before every sync, since if errors get to sync they'll
-- result in bad UI display, a C++ exception, or maybe even a segfault (but C++
-- args should be protected by ASSERTs).
--
-- If there was any need, Cmd.State verification could go here too.
verify_state :: State.State -> IO State.State
verify_state state = do
    let (res, logs) = State.verify state
    mapM_ Log.write logs
    case res of
        Left err -> error $ "fatal state consistency error: " ++ show err
        Right state2 -> return state2


-- * derive events

run_derive :: State.State -> State.State -> Cmd.State -> [Update.Update]
    -> IO Cmd.State
run_derive ui_from ui_to cmd_state updates = do
    (cmd_state, _, logs, result) <- Cmd.run_io
        ui_to cmd_state (derive_events ui_from ui_to updates >> return Cmd.Done)
    mapM_ Log.write logs
    case result of
        Left err -> Log.error $ "ui error deriving: " ++ show err
        _ -> return ()
    return cmd_state

derive_events :: State.State -> State.State -> [Update.Update] -> Cmd.CmdT IO ()
derive_events ui_from ui_to updates = do
    old_threads <- Cmd.gets Cmd.state_derive_threads
    let block_ids = dirty_blocks ui_from ui_to updates
    -- In case they aren't done, their work is about to be obsolete.
    Trans.liftIO $ mapM_ Concurrent.killThread $
        Seq.map_maybe (flip Map.lookup old_threads) block_ids
    threads <- mapM background_derive block_ids
    let new_threads = Map.fromList (zip block_ids threads)
    Cmd.modify_state $ \st -> st { Cmd.state_derive_threads =
        Map.union new_threads (Map.delete_keys block_ids old_threads) }

background_derive :: BlockId -> Cmd.CmdT IO Concurrent.ThreadId
background_derive block_id = do
    st <- Cmd.get_state
    perf <- Play.perform
        block_id (Cmd.state_instrument_db st) (Cmd.state_schema_map st)
    Cmd.put_state $ st { Cmd.state_performance =
        Map.insert block_id perf (Cmd.state_performance st) }
    Trans.liftIO $ Thread.start_thread ("derive " ++ show block_id) $
        evaluate_performance block_id perf

-- | Figure out which blocks should be re-derived.
dirty_blocks :: State.State -> State.State -> [Update.Update] -> [BlockId]
dirty_blocks ui_from ui_to updates = Seq.unique (track_block_ids ++ block_ids)
    where
    block_ids = Seq.map_maybe Update.block_changed updates
    track_block_ids = concatMap blocks_of track_ids
    -- When track title changes come from the UI they aren't emitted as
    -- Updates, but they should still trigger a re-derive.
    track_ids = Seq.map_maybe Update.track_changed
        (Diff.track_diff ui_from ui_to ++ updates)
    blocks_of tid = map fst $ State.find_tracks
        ((== Just tid) . Block.track_id_of) (State.state_blocks ui_to)


evaluate_performance :: BlockId -> Cmd.Performance -> IO ()
evaluate_performance block_id perf = do
    -- Force the performance to actually be evaluated.  Writing out the logs
    -- should do it.
    let prefix = Text.append (Text.pack ("deriving " ++ show block_id ++ ": "))
    let logs = map (\log -> log { Log.msg_text = prefix (Log.msg_text log) })
            (Cmd.perf_logs perf)
    mapM_ Log.write logs


-- * derive signals

derive_signals :: Schema.SchemaMap -> State.State
    -> (Sync.BlockSamples, [Log.Msg])
derive_signals schema_map ui_state = (block_samples, logs)
    where
    block_ids = Map.keys (State.state_blocks ui_state)
    track_results = zip block_ids $
        map (State.eval ui_state . derive_signal schema_map) block_ids

    block_samples = [(block_id, track_samples)
        | (block_id, Right (track_samples, _)) <- track_results]
    logs = [warn block_id err | (block_id, Left err) <- track_results]
        ++ concat [logs | (_, Right (_, logs)) <- track_results]
    warn block_id err = Log.msg Log.Warn $
        "exception deriving signal for " ++ show block_id ++ ": " ++ show err

derive_signal :: (State.UiStateMonad m) =>
    Schema.SchemaMap -> BlockId -> m (Track.TrackSamples, [Log.Msg])
derive_signal schema_map block_id = do
    ui_state <- State.get
    deriver <- Schema.get_signal_deriver schema_map block_id
    let (result, _, _, logs, _) = Derive.derive
            -- Signal derivation doesn't do calls, so I can pass an empty map.
            Derive.empty_lookup_deriver ui_state Derive.empty_call_map True
            (Derive.with_stack_block block_id deriver)
    case result of
        Left err -> State.throw (show err)
        Right sig ->
            return (map (Arrow.second Signal.to_track_samples) sig, logs)
