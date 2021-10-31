-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Subset of the responder that handles syncing from Ui.State to the UI.
-}
module Cmd.ResponderSync (Sync, sync) where
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Debug as Debug
import qualified Util.Log as Log
import qualified Util.Trace as Trace

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Integrate as Integrate
import qualified Cmd.Internal as Internal

import qualified Ui.Diff as Diff
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import           Global


type Sync = Track.TrackSignals -> Track.SetStyleHigh -> Ui.State
    -> [Update.DisplayUpdate] -> IO (Maybe Ui.Error)

-- | Sync @ui_to@ to the UI.
--
-- Returns both UI state and cmd state since verification may clean up the UI
-- state, and this is where the undo history is stored in Cmd.State.
sync :: Sync
    -> Ui.State -- ^ state before Cmd was run
    -> Ui.State -- ^ current state
    -> Cmd.State -> Update.UiDamage -> MVar.MVar Ui.State
    -> IO ([Update.UiUpdate], Ui.State)
    -- ^ Sync uses 'Update.DisplayUpdate's, but the diff also produces
    -- UiUpdates, which are needed for incremental save and score damage.
sync sync_func ui_from ui_to cmd_to ui_damage play_monitor_state = do
    ui_to <- case Ui.quick_verify ui_damage ui_to of
        Left err -> do
            Log.error $ "cmd caused a verify error, rejecting state change: "
                <> txt err
            return ui_from
        Right (state, warns) -> do
            unless (null warns) $
                Log.warn $ "verify fixed issues: "
                    <> Text.intercalate "; " warns
            return state
    Trace.trace "sync.verify"

    let (ui_updates, display_updates) = Diff.diff ui_damage ui_from ui_to
    Trace.force (ui_updates, display_updates)
    Trace.trace "sync.diff"
    -- Debug.fullM (Debug.putp "ui_damage") ui_damage
    -- Debug.fullM (Debug.putp "ui_updates") ui_updates
    -- Debug.fullM (Debug.putp "display_updates") display_updates

    (ui_to, ui_updates, display_updates) <-
        case Integrate.score_integrate ui_updates ui_to of
            Left err -> do
                Log.error $ "score_integrate failed: " <> pretty err
                return (ui_from, ui_updates, display_updates)
            Right (logs, state, updates) -> do
                mapM_ Log.write logs
                let (ui_updates', display_updates') =
                        Diff.diff updates ui_to state
                return
                    ( state
                    , ui_updates ++ ui_updates'
                    , display_updates ++ display_updates'
                    )
    Trace.force (ui_updates, display_updates)
    Trace.trace "sync.score_integrate"

    when (any modified_view ui_updates) $
        MVar.modifyMVar_ play_monitor_state (const (return ui_to))
    when (Cmd.state_debug cmd_to && not (null display_updates)) $
        Debug.putp "updates" display_updates
    err <- sync_func (get_track_signals cmd_to) Internal.set_style
        ui_to display_updates
    Trace.trace "sync.sync"
    whenJust err $ \err ->
        Log.error $ "syncing updates: " <> pretty err
    return (ui_updates, ui_to)

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
