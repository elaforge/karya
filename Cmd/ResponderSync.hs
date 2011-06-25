{- | Subset of the responder that handles syncing from State.State to the UI.
-}
module Cmd.ResponderSync (Sync, sync) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Log as Log
import Ui
import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Performance as Performance


type Sync = Track.TrackSignals -> State.State -> [Update.Update]
    -> IO (Maybe State.StateError)

-- | Sync @state2@ to the UI.
--
-- Returns both UI state and cmd state since verification may clean up the UI
-- state, and this is where the undo history is stored in Cmd.State.
sync :: Sync -> Performance.SendStatus
    -> State.State -- ^ state before UiMsg.UiUpdates have been applied
    -> State.State -- ^ state before Cmd was run
    -> State.State -- ^ current state
    -> Cmd.State -> [Update.Update]
    -> IO ([Update.Update], State.State, Cmd.State)
sync sync_func send_status ui_pre ui_from ui_to cmd_state cmd_updates = do
    -- I'd catch problems closer to their source if I did this from run_cmds,
    -- but it's nice to see that it's definitely happening before syncs.
    ui_to <- verify_state ui_to

    updates <- case Diff.diff cmd_updates ui_from ui_to of
        Left err -> Log.error ("diff error: " ++ err) >> return []
        Right updates -> do
            -- unless (null updates) $
            --     Trans.liftIO $ putStrLn $ "update: " ++ PPrint.pshow updates
            let tsigs = get_track_signals (State.state_root ui_to) cmd_state
            err <- sync_func tsigs ui_to updates
            case err of
                Nothing -> return ()
                Just err -> Log.error $ "syncing updates: " ++ show err
            return updates

    -- Kick off the background derivation threads.
    cmd_state <- Performance.update_performance
        send_status ui_pre ui_to cmd_state updates
    return (updates, ui_to, cmd_state)

get_track_signals :: Maybe BlockId -> Cmd.State -> Track.TrackSignals
get_track_signals maybe_root st = Maybe.fromMaybe Map.empty $ do
    root <- maybe_root
    pthread <- Map.lookup root (Cmd.state_performance_threads st)
    return $ Cmd.perf_track_signals (Cmd.pthread_perf pthread)

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
