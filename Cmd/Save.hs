{- | Functions to save and restore state to and from files.
-}
module Cmd.Save where
import qualified Data.Map as Map
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize as Serialize
import qualified Cmd.ViewConfig as ViewConfig


get_save_file :: (State.M m) => m FilePath
get_save_file = do
    state <- State.get
    return $ FilePath.combine (State.save_dir state)
        (State.save_name state ++ ".state")

-- * plain serialize

cmd_save :: FilePath -> Cmd.CmdT IO ()
cmd_save fname = do
    ui_state <- State.get
    save <- liftIO $ Serialize.save_state (State.clear ui_state)
    Log.notice $ "write state to " ++ show fname
    liftIO $ Serialize.serialize fname save

cmd_load :: FilePath -> Cmd.CmdT IO ()
cmd_load fname = do
    Log.notice $ "load state from " ++ show fname
    Serialize.SaveState state _ <- Cmd.require_right
        (("load " ++ fname ++ ": ") ++)
        =<< liftIO (Serialize.unserialize fname)
    set_state state

-- | Try to guess whether the given path is a git save or state save.  If it's
-- a directory, look inside for a .git or .state save.
cmd_load_any :: FilePath -> Cmd.CmdT IO ()
cmd_load_any path
    | SaveGit.is_git path = cmd_load_git path Nothing
    | otherwise = ifM (isdir path) look_in_dir (cmd_load path)
    where
    look_in_dir =
        ifM (isdir git) (cmd_load_git git Nothing) $
        ifM (isfile state) (cmd_load (path </> name ++ ".state")) $
        Cmd.throw $ "directory contains neither " ++ git ++ " nor " ++ state
        where
        git = path </> name ++ ".git"
        state = path </> name ++ ".state"
    name = FilePath.takeFileName path
    isdir = liftIO . Directory.doesDirectoryExist
    isfile = liftIO . Directory.doesFileExist

-- * git serialize

-- | Save a SavePoint to a git repo.
--
-- Unlike 'cmd_save', this doesn't take a FilePath.  This is because
-- 'Cmd.Undo.maintain_history' will be saving to the repo set by the
-- 'State.config_project_dir' and 'State.config_namespace'.  So if you want to
-- start saving in a new place, you should change those values.
cmd_save_git :: Cmd.CmdT IO ()
cmd_save_git = do
    state <- State.get
    let repo = SaveGit.save_repo state
    cmd_state <- Cmd.get
    let prev_commit = Cmd.hist_last_commit $ Cmd.state_history_config cmd_state
    (commit, save) <- Cmd.require_right (("save git " ++ repo ++ ": ") ++)
        =<< liftIO (SaveGit.save repo state prev_commit)
    Log.notice $ "wrote save " ++ show save ++ " to " ++ show repo
    Cmd.modify $ \st -> st
        { Cmd.state_history_config = (Cmd.state_history_config st)
            { Cmd.hist_last_save = Just save
            , Cmd.hist_last_commit = Just commit
            }
        }

cmd_load_git :: FilePath -> Maybe SaveGit.Commit -> Cmd.CmdT IO ()
cmd_load_git repo maybe_commit = do
    (state, commit, names) <- Cmd.require_right
        (("load git " ++ repo ++ ": ") ++)
        =<< liftIO (SaveGit.load repo maybe_commit)
    last_save <- rethrow "read_last_save" $
        SaveGit.read_last_save repo (Just commit)
    Log.notice $ "loaded from " ++ show repo ++ ", at " ++ Pretty.pretty commit
    set_state state
    Cmd.modify $ \st -> st
        { Cmd.state_history = (Cmd.state_history st)
            { Cmd.hist_last_cmd = Just $ Cmd.Load (Just commit) names }
        , Cmd.state_history_config = (Cmd.state_history_config st)
            { Cmd.hist_last_save = last_save }
        }

-- | Revert to given save point, or the last one.
cmd_revert :: Maybe String -> Cmd.CmdT IO ()
cmd_revert maybe_ref = do
    repo <- State.gets SaveGit.save_repo
    save <- case maybe_ref of
        Nothing -> Cmd.require_msg "no last save"
            =<< liftIO (SaveGit.read_last_save repo Nothing)
        Just ref -> Cmd.require_msg ("unparseable SavePoint: " ++ show ref) $
            SaveGit.ref_to_save ref
    commit <- Cmd.require_msg ("save ref not found: " ++ show save)
        =<< rethrow "cmd_revert" (SaveGit.read_save_ref repo save)
    Log.notice $ "revert to " ++ show save
    cmd_load_git repo (Just commit)

rethrow :: String -> IO a -> Cmd.CmdT IO a
rethrow caller io =
    Cmd.require_right id =<< liftIO (SaveGit.try caller io)

-- * misc

set_state :: State.State -> Cmd.CmdT IO ()
set_state state = do
    Play.cmd_stop
    Cmd.modify $ Cmd.reinit_state (Cmd.empty_history_entry state)
    State.put (State.clear state)
    root <- case State.config_root (State.state_config state) of
        Nothing -> return Nothing
        Just root -> Seq.head . Map.keys <$> State.views_of root
    let focused = msum [root, Seq.head (Map.keys (State.state_views state))]
    when_just focused ViewConfig.bring_to_front

cmd_save_midi_config :: FilePath -> Cmd.CmdT IO ()
cmd_save_midi_config fname = do
    config <- State.get_config id
    Log.notice $ "write midi config to " ++ show fname
    liftIO $ Serialize.serialize_pretty_text fname
        (State.config_midi config)

cmd_load_midi_config :: FilePath -> Cmd.CmdT IO ()
cmd_load_midi_config fname = do
    Log.notice $ "load midi config from " ++ show fname
    config <- Cmd.require_right
        (("unserializing midi config " ++ show fname ++ ": ") ++)
        =<< liftIO (Serialize.unserialize_text fname)
    State.set_midi_config config
