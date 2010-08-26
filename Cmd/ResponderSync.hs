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
module Cmd.ResponderSync (sync) where
import Control.Monad
import qualified Util.Log as Log

import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Performance as Performance


-- | Sync @state2@ to the UI.
-- Returns both UI state and cmd state since verification may clean up the UI
-- state, and this is where the undo history is stored in Cmd.State.
sync :: Performance.SendStatus -> State.State -> State.State -> Cmd.State
    -> [Update.Update] -> IO ([Update.Update], State.State, Cmd.State)
sync send_status ui_from ui_to cmd_state cmd_updates = do
    -- I'd catch problems closer to their source if I did this from run_cmds,
    -- but it's nice to see that it's definitely happening before syncs.
    ui_to <- verify_state ui_to

    diff_updates <- case Diff.diff ui_from ui_to of
        Left err -> Log.error ("diff error: " ++ err) >> return []
        Right diff_updates -> do
            unless (null diff_updates && null cmd_updates) $
                Log.debug $ "diff_updates: " ++ show diff_updates
                    ++ " cmd_updates: " ++ show cmd_updates
            Log.timer "got diff updates"
            err <- Sync.sync ui_to (diff_updates ++ cmd_updates)
            case err of
                Nothing -> return ()
                Just err -> Log.error $ "syncing updates: " ++ show err
            return diff_updates

    let updates = diff_updates ++ cmd_updates
    -- Kick off the background derivation threads.
    cmd_state <- Performance.update_performance
        send_status ui_from ui_to cmd_state updates
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
