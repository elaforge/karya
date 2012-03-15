{- | Functions to save and restore state to and from files.
-}
module Cmd.Save where
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified System.FilePath as FilePath

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Play as Play
import qualified Cmd.Serialize as Serialize
import qualified Cmd.ViewConfig as ViewConfig


get_save_file :: (Cmd.M m) => m FilePath
get_save_file = do
    dir <- State.get_config State.config_project_dir
    ns <- State.get_namespace
    return $ FilePath.combine dir (map sanitize (Id.un_namespace ns))
    where
    -- Shouldn't be necessary because of the Namespace restrictions.
    sanitize c = if FilePath.isPathSeparator c then '_' else c

cmd_save :: FilePath -> Cmd.CmdT IO ()
cmd_save fname = do
    ui_state <- State.get
    save <- Trans.liftIO $ Serialize.save_state (State.clear ui_state)
    Log.notice $ "write state to " ++ show fname
    Trans.liftIO $ Serialize.serialize fname save

cmd_load :: FilePath -> Cmd.CmdT IO ()
cmd_load fname = do
    Log.notice $ "load state from " ++ show fname
    try_state <- Trans.liftIO $ Serialize.unserialize fname
    Serialize.SaveState state _ <- either (\err -> Cmd.throw $
            "unserializing " ++ show fname ++ ": " ++ err)
        return try_state
    Log.notice $ "state loaded from " ++ show fname

    Play.cmd_stop
    Cmd.modify Cmd.reinit_state
    State.modify (const (State.clear state))
    root <- case State.config_root (State.state_config state) of
        Nothing -> return Nothing
        Just root -> Seq.head . Map.keys <$> State.get_views_of root
    let focused = msum [root, Seq.head (Map.keys (State.state_views state))]
    when_just focused ViewConfig.bring_to_front
    Edit.initialize_state

cmd_save_midi_config :: FilePath -> Cmd.CmdT IO ()
cmd_save_midi_config fname = do
    config <- State.get_config id
    Log.notice $ "write midi config to " ++ show fname
    Trans.liftIO $ Serialize.serialize_pretty_text fname
        (State.config_midi config)

cmd_load_midi_config :: FilePath -> Cmd.CmdT IO ()
cmd_load_midi_config fname = do
    Log.notice $ "load midi config from " ++ show fname
    try_config <- Trans.liftIO $ Serialize.unserialize_text fname
    config <- either (\exc -> Cmd.throw $
            "unserializing midi config " ++ show fname ++ ": " ++ show exc)
        return try_config
    State.set_midi_config config
