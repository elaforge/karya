-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Subset of the responder that handles syncing from State.State to the UI.
-}
module Cmd.ResponderSync (Sync, sync) where
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Integrate as Integrate
import qualified Cmd.Internal as Internal

import Global


type Sync = Track.TrackSignals -> Track.SetStyleHigh -> State.State
    -> [Update.DisplayUpdate] -> IO (Maybe State.Error)

-- | Sync @ui_to@ to the UI.
--
-- Returns both UI state and cmd state since verification may clean up the UI
-- state, and this is where the undo history is stored in Cmd.State.
sync :: Sync
    -> State.State -- ^ state before Cmd was run
    -> State.State -- ^ current state
    -> Cmd.State -> [Update.CmdUpdate] -> MVar.MVar State.State
    -> IO ([Update.UiUpdate], State.State, Cmd.State)
sync sync_func ui_from ui_to cmd_state cmd_updates play_monitor_state = do
    ui_to <- case State.quick_verify ui_to of
        Left err -> do
            Log.error $ "cmd caused a verify error, rejecting state change: "
                <> txt err
            return ui_from
        Right (state, warns) -> do
            unless (null warns) $
                Log.warn $ "verify fixed issues: "
                    <> Text.intercalate "; " warns
            return state

    let (ui_updates, display_updates) = Diff.diff cmd_updates ui_from ui_to
    -- Debug.fullM Debug.putp "ui_updates" ui_updates
    -- Debug.fullM Debug.putp "display_updates" display_updates
    (ui_to, ui_updates, display_updates) <-
        case Integrate.score_integrate ui_updates ui_to of
            Left err -> do
                Log.error $ "score_integrate failed: " <> prettyt err
                return (ui_from, ui_updates, display_updates)
            Right (logs, state, updates) -> do
                mapM_ Log.write logs
                let (ui_updates', display_updates') =
                        Diff.diff updates ui_to state
                -- Debug.fullM Debug.putp "int ui_updates" ui_updates'
                -- Debug.fullM Debug.putp "int display_updates" display_updates'
                return (state, ui_updates ++ ui_updates',
                    display_updates ++ display_updates')

    when (any modified_view ui_updates) $
        MVar.modifyMVar_ play_monitor_state (const (return ui_to))
    err <- sync_func (get_track_signals cmd_state) Internal.set_style
        ui_to display_updates
    whenJust err $ \err ->
        Log.error $ "syncing updates: " <> prettyt err
    return (ui_updates, ui_to, cmd_state)

-- | Get all track signals already derived.  TrackSignals are only collected
-- for top level derives, so there should only be signals for visible windows.
-- If the derive is still in progress, there may not be signals, but they will
-- be directly when the DeriveComplete is received.
get_track_signals :: Cmd.State -> Track.TrackSignals
get_track_signals = mconcatMap Cmd.perf_track_signals . Map.elems
    . Cmd.state_performance . Cmd.state_play

modified_view :: Update.UiUpdate -> Bool
modified_view (Update.View _ update) = case update of
    Update.CreateView {} -> True
    Update.DestroyView {} -> True
    _ -> False
modified_view _ = False
