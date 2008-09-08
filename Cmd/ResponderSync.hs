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
import qualified Control.Arrow as Arrow
import Control.Monad
import qualified Data.Map as Map

import qualified Util.Log as Log

import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.State as State
import qualified Ui.Update as Update
import qualified Ui.Sync as Sync
import qualified Ui.Diff as Diff

import qualified Cmd.Cmd as Cmd

import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema

import qualified Perform.Signal as Signal


-- | Sync @state2@ to the UI.
-- Returns both UI state and cmd state since verification may clean up the UI
-- state, and this is where the undo history is stored in Cmd.State.
sync :: Cmd.State -> State.State -> State.State
    -> [Update.Update] -> IO ([Update.Update], State.State)
sync cmd_state ui_from ui_to cmd_updates = do
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
    -- putStrLn $ "block samples: " ++ show block_samples
    diff_updates <- case Diff.diff ui_from ui_to of
        Left err -> Log.error ("diff error: " ++ err) >> return []
        Right diff_updates -> do
            unless ((null diff_updates) && (null cmd_updates)) $
                Log.debug $ "diff_updates: " ++ show diff_updates
                    ++ " cmd_updates: " ++ show cmd_updates
            err <- Sync.sync ui_to (diff_updates ++ cmd_updates) block_samples
            case err of
                Nothing -> return ()
                Just err -> Log.error $ "syncing updates: " ++ show err
            return diff_updates
    return (diff_updates ++ cmd_updates, ui_to)

-- | This should be run before every sync, since if errors get to sync they'll
-- result in bad UI display, a C++ exception, or maybe even a segfault (but C++
-- args should be protected by ASSERTs).
--
-- If there was any need, Cmd.State verification could go here too.
-- verify_state :: (Monad m) => Cmd.CmdT m ()
verify_state state = do
    let (res, logs) = State.verify state
    mapM_ Log.write logs
    case res of
        Left err -> error $ "fatal state consistency error: " ++ show err
        Right state2 -> return state2



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
    Schema.SchemaMap -> Block.BlockId -> m (Track.TrackSamples, [Log.Msg])
derive_signal schema_map block_id = do
    ui_state <- State.get
    deriver <- Schema.get_signal_deriver schema_map =<< State.get_block block_id
    let (result, _, _, logs, _) = Derive.derive
            Derive.empty_lookup_deriver ui_state True
            (Derive.with_stack_block block_id deriver)
    case result of
        Left err -> State.throw (show err)
        Right sig ->
            return (map (Arrow.second Signal.to_track_samples) sig, logs)
