{- | Functions to save and restore state to and from files.
-}
module Cmd.Save where
import qualified Data.Map as Map
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.Git as Git
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize as Serialize
import qualified Cmd.ViewConfig as ViewConfig

import qualified App.Config as Config


-- * universal

-- | Save to the current 'Cmd.state_save_file', or create a new git repo if
-- there is none.
cmd_save :: Cmd.CmdT IO ()
cmd_save = Cmd.gets Cmd.state_save_file >>= \x -> case x of
    Nothing -> cmd_save_git
    Just (Cmd.SaveGit _) -> cmd_save_git
    Just (Cmd.SaveState fn) -> cmd_save_state fn

-- | Try to guess whether the given path is a git save or state save.  If it's
-- a directory, look inside for a .git or .state save.
cmd_load :: FilePath -> Cmd.CmdT IO ()
cmd_load path
    | SaveGit.is_git path = cmd_load_git path Nothing
    | otherwise = ifM (isdir path) look_in_dir (cmd_load_state path)
    where
    look_in_dir =
        ifM (isdir git_fn) (cmd_load_git git_fn Nothing) $
        ifM (isfile state_fn) (cmd_load_state state_fn) $
        Cmd.throw $ "directory contains neither " ++ git_fn ++ " nor "
            ++ state_fn
        where
        git_fn = path </> default_git
        state_fn = path </> default_state
    isdir = Cmd.rethrow_io . Directory.doesDirectoryExist
    isfile = Cmd.rethrow_io . Directory.doesFileExist

-- * plain serialize

get_state_save :: (Cmd.M m) => m FilePath
get_state_save = do
    ns <- State.gets (State.config#State.namespace #$)
    state <- Cmd.get
    return $ state_save_file ns state

state_save_file :: Id.Namespace -> Cmd.State -> FilePath
state_save_file ns state = case Cmd.state_save_file state of
    Nothing -> Cmd.path state Config.save_dir </> Id.un_namespace ns
        </> default_state
    Just (Cmd.SaveState fn) -> fn
    Just (Cmd.SaveGit repo) -> FilePath.replaceExtension repo ".state"

default_state :: FilePath
default_state = "save.state"

-- | Save the state to the given file.  Unlike 'cmd_save_git', this doesn't
-- set 'Cmd.state_save_file'.
cmd_save_state :: FilePath -> Cmd.CmdT IO ()
cmd_save_state fname = do
    ui_state <- State.get
    save <- liftIO $ Serialize.save_state (State.clear ui_state)
    Log.notice $ "write state to " ++ show fname
    liftIO $ Serialize.serialize fname save

cmd_load_state :: FilePath -> Cmd.CmdT IO ()
cmd_load_state fname = do
    Log.notice $ "load state from " ++ show fname
    Serialize.SaveState state _ <- Cmd.require_right
        (("load " ++ fname ++ ": ") ++)
        =<< liftIO (Serialize.unserialize fname)
    set_state state
    Cmd.modify $ \st -> st
        { Cmd.state_save_file = Just $ Cmd.SaveState fname }

-- * git serialize

get_git_save :: (Cmd.M m) => m Git.Repo
get_git_save = do
    ns <- State.gets (State.config#State.namespace #$)
    state <- Cmd.get
    return $ git_save_file ns state

git_save_file :: Id.Namespace -> Cmd.State -> Git.Repo
git_save_file ns state = case Cmd.state_save_file state of
    Nothing -> Cmd.path state Config.save_dir </> Id.un_namespace ns
        </> default_git
    Just (Cmd.SaveState fn) -> FilePath.replaceExtension fn ".git"
    Just (Cmd.SaveGit repo) -> repo

default_git :: FilePath
default_git = "save.git"

-- | Save a SavePoint to the git repo in 'Cmd.state_save_file', or start a new
-- one.  Set the 'Cmd.state_save_file' to the repo, so I'll keep saving to
-- that repo.
--
-- This doesn't take a FilePath.  This is because 'Cmd.Undo.maintain_history'
-- will be saving to the repo set by 'Cmd.state_save_file'.
cmd_save_git :: Cmd.CmdT IO ()
cmd_save_git = do
    state <- State.get
    cmd_state <- Cmd.get
    let repo = git_save_file (State.config#State.namespace #$ state) cmd_state
    let prev_commit = Cmd.hist_last_commit $ Cmd.state_history_config cmd_state
    (commit, save) <- Cmd.require_right (("save git " ++ repo ++ ": ") ++)
        =<< liftIO (SaveGit.save repo state prev_commit)
    Log.notice $ "wrote save " ++ show save ++ " to " ++ show repo
    Cmd.modify $ \st -> st
        { Cmd.state_save_file = Just $ Cmd.SaveGit repo
        , Cmd.state_history = reset_history commit (Cmd.state_history st)
        , Cmd.state_history_config = (Cmd.state_history_config st)
            { Cmd.hist_last_save = Just save
            , Cmd.hist_last_commit = Just commit
            }
        }

-- | If I am starting a new repo, I have to clear out all the remains of the
-- old repo, since its Commits are no longer valid.
reset_history :: SaveGit.Commit -> Cmd.History -> Cmd.History
reset_history commit hist = hist
    { Cmd.hist_past = map clear (Cmd.hist_past hist)
    , Cmd.hist_present = (Cmd.hist_present hist)
        { Cmd.hist_commit = Just commit }
    , Cmd.hist_future = []
    }
    where clear entry = entry { Cmd.hist_commit = Nothing }

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
        { Cmd.state_save_file = Just $ Cmd.SaveGit repo
        , Cmd.state_history = (Cmd.state_history st)
            { Cmd.hist_last_cmd = Just $ Cmd.Load (Just commit) names }
        , Cmd.state_history_config = (Cmd.state_history_config st)
            { Cmd.hist_last_save = last_save }
        }

-- | Revert to given save point, or the last one.
cmd_revert :: Maybe String -> Cmd.CmdT IO ()
cmd_revert maybe_ref = do
    save_file <- Cmd.require_msg "can't revert when there is no save file"
        =<< Cmd.gets Cmd.state_save_file
    case save_file of
        Cmd.SaveState fn -> do
            when_just maybe_ref $ Cmd.throw .
                ("can't revert to a commit when the save file isn't git: " ++)
            cmd_load fn
        Cmd.SaveGit repo -> revert_git repo
    Log.notice $ "revert to " ++ show save_file
    where
    revert_git repo = do
        save <- case maybe_ref of
            Nothing -> Cmd.require_msg "no last save"
                =<< liftIO (SaveGit.read_last_save repo Nothing)
            Just ref -> Cmd.require_msg ("unparseable SavePoint: " ++ show ref)
                (SaveGit.ref_to_save ref)
        commit <- Cmd.require_msg ("save ref not found: " ++ show save)
            =<< rethrow "cmd_revert" (SaveGit.read_save_ref repo save)
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
