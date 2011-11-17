{- | Subset of the responder that handles syncing from State.State to the UI.
-}
module Cmd.ResponderSync (Sync, sync) where
import qualified Control.Concurrent.MVar as MVar
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Performance as Performance
import Types


type Sync = Track.TrackSignals -> State.State -> [Update.DisplayUpdate]
    -> IO (Maybe State.StateError)

-- | Sync @state2@ to the UI.
--
-- Returns both UI state and cmd state since verification may clean up the UI
-- state, and this is where the undo history is stored in Cmd.State.
sync :: Sync -> Performance.SendStatus
    -> State.State -- ^ state before UiMsg.UiUpdates have been applied
    -> State.State -- ^ state before Cmd was run
    -> State.State -- ^ current state
    -> Cmd.State -> [Update.CmdUpdate] -> MVar.MVar State.State
    -> IO ([Update.CmdUpdate], State.State, Cmd.State)
sync sync_func send_status ui_pre ui_from ui_to cmd_state cmd_updates
        updater_state = do
    -- I'd catch problems closer to their source if I did this from run_cmds,
    -- but it's nice to see that it's definitely happening before syncs.
    verify_state ui_to

    cmd_updates <- case Diff.diff cmd_updates ui_from ui_to of
        Left err -> Log.error ("diff error: " ++ err) >> return []
        Right (cmd_updates, display_updates) -> do
            -- unless (null display_updates) $
            --     Trans.liftIO $ putStrLn $ "update: "
            --          ++ PPrint.pshow display_updates
            when (any modified_view cmd_updates) $
                MVar.modifyMVar_ updater_state (const (return ui_to))
            let tsigs = get_track_signals
                    (State.config_root (State.state_config ui_to)) cmd_state
            err <- sync_func tsigs ui_to display_updates
            case err of
                Nothing -> return ()
                Just err -> Log.error $ "syncing updates: " ++ show err
            return cmd_updates

    -- Kick off the background derivation threads.
    cmd_state <- Performance.update_performance
        Performance.default_derive_wait send_status ui_pre ui_to cmd_state
        cmd_updates
    return (cmd_updates, ui_to, cmd_state)

get_track_signals :: Maybe BlockId -> Cmd.State -> Track.TrackSignals
get_track_signals maybe_root st = Maybe.fromMaybe Map.empty $ do
    root <- maybe_root
    Cmd.perf_track_signals <$>
        (Map.lookup root $ Cmd.state_performance (Cmd.state_play st))

-- | This should be run before every sync, since if errors get to sync they'll
-- result in bad UI display, a C++ exception, or maybe even a segfault (but C++
-- args should be protected by ASSERTs).
--
-- If there were a need, Cmd.State verification could go here too.
verify_state :: State.State -> IO ()
verify_state state = when_just (State.verify state) $ \err ->
    Log.error $ "state error while verifying: " ++ show err

modified_view :: Update.CmdUpdate -> Bool
modified_view (Update.ViewUpdate _ update) = case update of
    Update.CreateView {} -> True
    Update.DestroyView {} -> True
    _ -> False
modified_view _ = False
