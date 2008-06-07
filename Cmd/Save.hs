{- | Functions to save and restore state to and from files.
-}
module Cmd.Save where
import qualified Control.Monad.Trans as Trans
import qualified System.IO as IO

import qualified Util.Log as Log

import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Selection as Selection
import qualified Cmd.Serialize as Serialize

import qualified App.Config as Config


cmd_save :: (Trans.MonadIO m) => Maybe FilePath -> Cmd.CmdT m ()
cmd_save maybe_fname = do
    cmd_state <- Cmd.get_state
    ui_state <- State.get
    let fname = maybe (Cmd.state_default_save_file cmd_state) id maybe_fname
    save <- Trans.liftIO $ Serialize.save_state ui_state
    Log.notice $ "write state to " ++ show fname
    Trans.liftIO $ Serialize.serialize_text (fname ++ ".text") save
    Trans.liftIO $ Serialize.serialize fname save

cmd_load :: (Trans.MonadIO m) => Maybe FilePath -> Cmd.CmdT m ()
cmd_load maybe_fname = do
    cmd_state <- Cmd.get_state
    let fname = maybe (Cmd.state_default_save_file cmd_state) id maybe_fname
    Log.notice $ "read state from " ++ show fname
    try_state <- Trans.liftIO $ Serialize.unserialize fname
    state <- case try_state of
        Left exc -> State.throw $
            "error unserializing " ++ show fname ++ ": " ++ show exc
        Right st -> return st

    State.modify (const (Serialize.save_ui_state state))
    initialize_state

-- | Sync UI state up with Cmd state and schedule UI updates.
initialize_state :: (Monad m) => Cmd.CmdT m ()
initialize_state = do
    -- TODO these scattered sync functions are kinda grody.  Isn't there a
    -- better way to keep track of state that needs to be synced?  Or avoid
    -- doing it in the first place?
    Edit.sync_edit_box_status
    mapM_ (flip State.set_edit_box Config.box_color) =<< State.get_all_block_ids
    Edit.sync_octave_status
    Edit.sync_step_status
    mapM_ Selection.sync_selection_status =<< State.get_all_view_ids
    mapM_ Cmd.sync_zoom_status =<< State.get_all_view_ids
    -- Emit track updates for all tracks, since I don't know where events have
    -- changed.
    State.update_all_tracks
