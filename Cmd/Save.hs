{- | Functions to save and restore state to and from files.
-}
module Cmd.Save where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans as Trans
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Directory as Directory

import qualified Util.Log as Log

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Selection as Selection


version_string = "seq state, 1"

cmd_save :: (Trans.MonadIO m) => Maybe FilePath -> Cmd.CmdT m ()
cmd_save maybe_fname = do
    cmd_state <- Cmd.get_state
    ui_state <- State.get
    let fname = maybe (Cmd.state_default_save_file cmd_state) id maybe_fname
    Log.notice $ "write state to " ++ show fname
    Trans.liftIO $ write fname (version_string ++ "\n"
        ++ serialize_ui_state ui_state ++ "\n")

cmd_load :: (Trans.MonadIO m) => Maybe FilePath -> Cmd.CmdT m ()
cmd_load maybe_fname = do
    cmd_state <- Cmd.get_state
    let fname = maybe (Cmd.state_default_save_file cmd_state) id maybe_fname
    Log.notice $ "read state from " ++ show fname
    [_ver, ui_str] <- fmap lines (Trans.liftIO (IO.readFile fname))

    either_ui_state <- Trans.liftIO $ Exception.try $
        Exception.evaluate (deserialize_ui_state ui_str)
    ui_state <- case either_ui_state of
        Left exc -> State.throw $
            "error deserializing " ++ show fname ++ ": " ++ show exc
        Right st -> return st

    State.modify (const ui_state)
    initialize_state

enoent_exc exc = case Exception.ioErrors exc of
    Just io_error | IO.Error.isDoesNotExistError io_error -> Just io_error
    _ -> Nothing

write filename s = do
    Exception.catchJust enoent_exc
        (Directory.renameFile filename (filename ++ ".last"))
        (\_exc -> return ())
    IO.writeFile filename s

-- | Sync UI state up with Cmd state and schedule UI updates.
initialize_state :: (Monad m) => Cmd.CmdT m ()
initialize_state = do
    -- TODO these scattered sync functions are kinda grody.  Isn't there a
    -- better way to keep track of state that needs to be synced?  Or avoid
    -- doing it in the first place?
    Edit.sync_edit_box_status
    Edit.sync_octave_status
    Edit.sync_step_status
    mapM_ Selection.sync_selection_status =<< State.get_all_view_ids
    mapM_ Cmd.sync_zoom_status =<< State.get_all_view_ids
    -- Emit track updates for all tracks, since I don't know where events have
    -- changed.
    State.update_all_tracks

serialize_ui_state :: State.State -> String
serialize_ui_state st = show st

deserialize_ui_state :: String -> State.State
deserialize_ui_state s = read s
