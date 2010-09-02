{- | Subset of the responder that handles syncing from State.State to the UI.
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
        Left err -> do
            Log.error $ "state error while verifying: " ++ show err
            return state
        Right state2 -> return state2
