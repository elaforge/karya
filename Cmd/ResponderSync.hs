-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Subset of the responder that handles syncing from State.State to the UI.
-}
module Cmd.ResponderSync (Sync, sync) where
import qualified Control.Concurrent.MVar as MVar
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Internal as Internal
import qualified Cmd.Performance as Performance

import Types


type Sync = Track.TrackSignals -> Track.SetStyleHigh -> State.State
    -> [Update.DisplayUpdate] -> IO (Maybe State.Error)

-- | Sync @ui_to@ to the UI.
--
-- Returns both UI state and cmd state since verification may clean up the UI
-- state, and this is where the undo history is stored in Cmd.State.
sync :: Sync -> Performance.SendStatus
    -> State.State -- ^ state before UiMsg.UiUpdates have been applied
    -> State.State -- ^ state before Cmd was run
    -> State.State -- ^ current state
    -> Cmd.State -> [Update.CmdUpdate] -> MVar.MVar State.State
    -> IO ([Update.UiUpdate], State.State, Cmd.State)
sync sync_func send_status ui_pre ui_from ui_to cmd_state cmd_updates
        play_monitor_state = do
    ui_to <- case State.quick_verify ui_to of
        Left err -> do
            Log.error $ "cmd caused a verify error, rejecting state change: "
                ++ err
            return ui_from
        Right (state, warns) -> do
            unless (null warns) $
                Log.warn $ "verify fixed issues: " ++ Seq.join "; " warns
            return state

    let (ui_updates, display_updates) = Diff.diff cmd_updates ui_from ui_to
    -- Debug.fullM Debug.putp "ui_updates" ui_updates
    -- Debug.fullM Debug.putp "display_updates" display_updates

    when (any modified_view ui_updates) $
        MVar.modifyMVar_ play_monitor_state (const (return ui_to))
    let tsigs = get_track_signals
            (State.config_root (State.state_config ui_to)) cmd_state
    err <- sync_func tsigs Internal.set_style ui_to display_updates
    whenJust err $ \err ->
        Log.error $ "syncing updates: " ++ Pretty.pretty err

    -- Kick off the background derivation threads.
    cmd_state <- Performance.update_performance send_status ui_pre ui_to
        cmd_state ui_updates
    return (ui_updates, ui_to,
        cmd_state { Cmd.state_derive_immediately = mempty })

get_track_signals :: Maybe BlockId -> Cmd.State -> Track.TrackSignals
get_track_signals maybe_root st = fromMaybe Map.empty $ do
    root <- maybe_root
    Cmd.perf_track_signals <$>
        Map.lookup root (Cmd.state_performance (Cmd.state_play st))

modified_view :: Update.UiUpdate -> Bool
modified_view (Update.View _ update) = case update of
    Update.CreateView {} -> True
    Update.DestroyView {} -> True
    _ -> False
modified_view _ = False
