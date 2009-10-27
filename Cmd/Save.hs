{- | Functions to save and restore state to and from files.
-}
module Cmd.Save where
import qualified Control.Monad.Trans as Trans
import qualified Data.List as List
import qualified System.FilePath as FilePath

import qualified Util.Log as Log

import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Serialize as Serialize


get_save_file :: (Monad m) => Cmd.CmdT m FilePath
get_save_file = do
    dir <- fmap State.state_project_dir State.get
    ns <- State.get_project
    return $ FilePath.combine dir (map sanitize ns ++ ".state")
    where sanitize c = if FilePath.isPathSeparator c then '_' else c

cmd_save :: (Trans.MonadIO m) => FilePath -> Cmd.CmdT m ()
cmd_save fname = do
    ui_state <- State.get
    save <- Trans.liftIO $ Serialize.save_state ui_state
    Log.notice $ "write state to " ++ show fname
    -- For the moment, also serialize to plain text, since that's easier to
    -- read and edit.
    Trans.liftIO $ Serialize.serialize_text (fname ++ ".text") save
    Trans.liftIO $ Serialize.serialize fname save

cmd_load :: (Trans.MonadIO m) => FilePath -> Cmd.CmdT m ()
cmd_load fname = do
    Log.notice $ "load state from " ++ show fname
    let unserialize = if ".text" `List.isSuffixOf` fname
            then Serialize.unserialize_text else Serialize.unserialize
    try_state <- Trans.liftIO $ unserialize fname
    state <- either (\exc -> Cmd.throw $
            "unserializing " ++ show fname ++ ": " ++ show exc)
        return try_state
    Log.notice $ "state loaded from " ++ show fname

    State.modify (const (Serialize.save_ui_state state))
    Edit.initialize_state
    Edit.clear_history

cmd_save_midi_config :: (Trans.MonadIO m) => FilePath -> Cmd.CmdT m ()
cmd_save_midi_config fname = do
    st <- State.get
    Log.notice $ "write midi config to " ++ show fname
    Trans.liftIO $ Serialize.serialize_pretty_text fname
        (State.state_midi_config st)

cmd_load_midi_config :: (Trans.MonadIO m) => FilePath -> Cmd.CmdT m ()
cmd_load_midi_config fname = do
    Log.notice $ "load midi config from " ++ show fname
    try_config <- Trans.liftIO $ Serialize.unserialize_text fname
    config <- either (\exc -> Cmd.throw $
            "unserializing midi config " ++ show fname ++ ": " ++ show exc)
        return try_config
    State.set_midi_config config
