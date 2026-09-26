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
sync syncFunc uiFrom uiTo cmdTo uiDamage playMonitorState = do
    uiTo <- case Ui.quickVerify uiDamage uiTo of
        Left err -> do
            Log.error $ "cmd caused a verify error, rejecting state change: "
                <> txt err
            pure uiFrom
        Right (state, warns) -> do
            unless (null warns) $
                Log.warn $ "verify fixed issues: "
                    <> Text.intercalate "; " warns
            pure state
    Trace.trace "sync.verify"

    let (uiUpdates, displayUpdates) = Diff.diff uiDamage uiFrom uiTo
    Trace.force (uiUpdates, displayUpdates)
    Trace.trace "sync.diff"
    -- Debug.fullM (Debug.putp "ui_damage") ui_damage
    -- Debug.fullM (Debug.putp "ui_updates") ui_updates
    -- Debug.fullM (Debug.putp "display_updates") display_updates

    (uiTo, uiUpdates, displayUpdates) <-
        case Integrate.scoreIntegrate uiUpdates uiTo of
            Left err -> do
                Log.error $ "score_integrate failed: " <> pretty err
                pure (uiFrom, uiUpdates, displayUpdates)
            Right (logs, state, updates) -> do
                mapM_ Log.write logs
                let (uiUpdates', displayUpdates') =
                        Diff.diff updates uiTo state
                pure
                    ( state
                    , uiUpdates ++ uiUpdates'
                    , displayUpdates ++ displayUpdates'
                    )
    Trace.force (uiUpdates, displayUpdates)
    Trace.trace "sync.score_integrate"

    when (any modifiedView uiUpdates) $
        MVar.modifyMVar_ playMonitorState (const (pure uiTo))
    when (Cmd.stateDebug cmdTo && not (null displayUpdates)) $
        Debug.putp "updates" displayUpdates
    err <- syncFunc (getTrackSignals cmdTo) Internal.setStyle
        uiTo displayUpdates
    Trace.trace "sync.sync"
    whenJust err $ \err ->
        Log.error $ "syncing updates: " <> pretty err
    pure (uiUpdates, uiTo)

-- | Get all track signals already derived.  TrackSignals are only collected
-- for top level derives, so there should only be signals for visible windows.
-- If the derive is still in progress, there may not be signals, but they will
-- be directly when the DeriveComplete is received.
getTrackSignals :: Cmd.State -> Track.TrackSignals
getTrackSignals = mconcatMap Cmd.perfTrackSignals . Map.elems
    . Cmd.statePerformance . Cmd.statePlay

modifiedView :: Update.UiUpdate -> Bool
modifiedView (Update.View _ update) = case update of
    Update.CreateView {} -> True
    Update.DestroyView {} -> True
    _ -> False
modifiedView _ = False
