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
save :: Cmd.CmdT IO ()
save = Cmd.gets Cmd.state_save_file >>= \x -> case x of
    Nothing -> save_git
    Just (Cmd.SaveGit repo) -> save_git_as repo
    Just (Cmd.SaveState fn) -> save_state_as fn

-- | Try to guess whether the given path is a git save or state save.  If it's
-- a directory, look inside for a .git or .state save.
load :: FilePath -> Cmd.CmdT IO ()
load path
    | SaveGit.is_git path = load_git path Nothing
    | otherwise = ifM (isdir path) look_in_dir (load_state path)
    where
    look_in_dir =
        ifM (isdir git_fn) (load_git git_fn Nothing) $
        ifM (isfile state_fn) (load_state state_fn) $
        Cmd.throw $ "directory contains neither " ++ git_fn ++ " nor "
            ++ state_fn
        where
        git_fn = path </> default_git
        state_fn = path </> default_state
    isdir = Cmd.rethrow_io . Directory.doesDirectoryExist
    isfile = Cmd.rethrow_io . Directory.doesFileExist

-- * plain serialize

save_state :: Cmd.CmdT IO ()
save_state = save_state_as =<< get_state_path

-- | Save the state to the given file and set 'Cmd.state_save_file'.
--
-- The directory of the filename will become the project directory, so things
-- like the saved REPL history and the ly subdirectory will go there.
save_state_as :: FilePath -> Cmd.CmdT IO ()
save_state_as fname = do
    write_state fname
    set_save_file (Right fname) False

write_state :: FilePath -> Cmd.CmdT IO ()
write_state fname = do
    ui_state <- State.get
    save <- liftIO $ Serialize.make_save_state (State.clear ui_state)
    Log.notice $ "write state to " ++ show fname
    liftIO $ Serialize.serialize fname save

load_state :: FilePath -> Cmd.CmdT IO ()
load_state fname = do
    Log.notice $ "load state from " ++ show fname
    let mkmsg = (("load " ++ fname ++ ": ") ++)
    Serialize.SaveState state _ <- Cmd.require_msg (mkmsg "doesn't exist")
        =<< Cmd.require_right mkmsg =<< liftIO (Serialize.unserialize fname)
    set_state (Right fname) True state

-- ** path

get_state_path :: (Cmd.M m) => m FilePath
get_state_path = do
    ns <- State.gets (State.config#State.namespace #$)
    state <- Cmd.get
    return $ make_state_path ns state

make_state_path :: Id.Namespace -> Cmd.State -> FilePath
make_state_path ns state = case Cmd.state_save_file state of
    Nothing -> Cmd.path state Config.save_dir </> Id.un_namespace ns
        </> default_state
    Just (Cmd.SaveState fn) -> fn
    Just (Cmd.SaveGit repo) -> FilePath.replaceExtension repo ".state"

default_state :: FilePath
default_state = "save.state"

-- * git serialize

-- | Save a SavePoint to the git repo in 'Cmd.state_save_file', or start a new
-- one.  Set the 'Cmd.state_save_file' to the repo, so I'll keep saving to
-- that repo.
save_git :: Cmd.CmdT IO ()
save_git = save_git_as =<< get_git_path

save_git_as ::
    SaveGit.Repo -- ^ Save to this repo, or create it.
    -- 'Cmd.Undo.maintain_history' will start checkpointing to it.
    -> Cmd.CmdT IO ()
save_git_as repo = do
    cmd_state <- Cmd.get
    let rethrow = Cmd.require_right ("save git " <> repo <> ": " <>)
    commit <- case Cmd.hist_last_commit $ Cmd.state_history_config cmd_state of
        Just commit -> return commit
        Nothing -> do
            state <- State.get
            rethrow =<< liftIO (SaveGit.checkpoint repo
                (SaveGit.SaveHistory state Nothing [] ["save"]))
    save <- rethrow =<< liftIO (SaveGit.set_save_tag repo commit)
    Log.notice $ "wrote save " ++ show save ++ " to " ++ show repo
    set_save_file (Left (commit, repo)) False

load_git :: FilePath -> Maybe SaveGit.Commit -> Cmd.CmdT IO ()
load_git repo maybe_commit = do
    (state, commit, names) <- Cmd.require_right
        (("load git " ++ repo ++ ": ") ++)
        =<< liftIO (SaveGit.load repo maybe_commit)
    Log.notice $ "loaded from " ++ show repo ++ ", at " ++ Pretty.pretty commit
    set_state (Left (commit, repo)) True state
    Cmd.modify $ \st -> st
        { Cmd.state_history = (Cmd.state_history st)
            -- This will cause "Cmd.Undo" to clear out the history, so
            -- 'set_save_file' as called by 'set_state' above is redundant.
            { Cmd.hist_last_cmd = Just $ Cmd.Load (Just commit) names }
        }

-- | Revert to given save point, or the last one.
revert :: Maybe String -> Cmd.CmdT IO ()
revert maybe_ref = do
    save_file <- Cmd.require_msg "can't revert when there is no save file"
        =<< Cmd.gets Cmd.state_save_file
    case save_file of
        Cmd.SaveState fn -> do
            when_just maybe_ref $ Cmd.throw .
                ("can't revert to a commit when the save file isn't git: " ++)
            load fn
        Cmd.SaveGit repo -> revert_git repo
    Log.notice $ "revert to " ++ show save_file
    where
    revert_git repo = do
        save <- case maybe_ref of
            Nothing -> fmap fst $ Cmd.require_msg "no last save"
                =<< liftIO (SaveGit.read_last_save repo Nothing)
            Just ref -> Cmd.require_msg ("unparseable SavePoint: " ++ show ref)
                (SaveGit.ref_to_save ref)
        commit <- Cmd.require_msg ("save ref not found: " ++ show save)
            =<< rethrow "revert" (SaveGit.read_save_ref repo save)
        load_git repo (Just commit)

rethrow :: String -> IO a -> Cmd.CmdT IO a
rethrow caller io =
    Cmd.require_right id =<< liftIO (SaveGit.try caller io)

-- ** path

get_git_path :: (Cmd.M m) => m Git.Repo
get_git_path = do
    ns <- State.gets (State.config#State.namespace #$)
    state <- Cmd.get
    return $ make_git_path ns state

make_git_path :: Id.Namespace -> Cmd.State -> Git.Repo
make_git_path ns state = case Cmd.state_save_file state of
    Nothing -> Cmd.path state Config.save_dir </> Id.un_namespace ns
        </> default_git
    Just (Cmd.SaveState fn) -> FilePath.replaceExtension fn ".git"
    Just (Cmd.SaveGit repo) -> repo

default_git :: FilePath
default_git = "save.git"

-- * misc

-- | Git repos don't checkpoint views, but because I'm accustomed to them
-- checkpointing everything else I expect the views to always be saved.
--
-- So call this when quitting or switching away from a save file to save the
-- views.
--
-- They could theoretically checkpoint view changes, but it would be
-- complicated (they mostly come from the GUI, not diff) and inefficient
-- (scrolling emits tons of them).
close_state :: Cmd.State -> State.State -> IO ()
close_state cmd_state ui_state = case Cmd.state_save_file cmd_state of
    Just (Cmd.SaveGit repo) ->
        SaveGit.save_views repo $ State.state_views ui_state
    _ -> return ()

type SaveFile = Either (SaveGit.Commit, SaveGit.Repo) FilePath

-- | If I switch away from a repo (either to another repo or to a plain state),
-- I have to clear out all the remains of the old repo, since its Commits are
-- no longer valid.
--
-- It's really important to call this whenever you change
-- 'Cmd.state_save_file'!
set_save_file :: (Cmd.M m) => SaveFile -> Bool -> m ()
set_save_file git_or_state clear_history = Cmd.modify $ \state -> state
    { Cmd.state_save_file = Just file
    , Cmd.state_history = let hist = Cmd.state_history state in hist
        { Cmd.hist_past = if clear_history then []
            else map clear (Cmd.hist_past hist)
        , Cmd.hist_present = (Cmd.hist_present hist)
            { Cmd.hist_commit = commit }
        , Cmd.hist_future = []
        }
    , Cmd.state_history_config = (Cmd.state_history_config state)
        { Cmd.hist_last_commit = commit }
    }
    where
    (commit, file) = case git_or_state of
        Left (commit, repo) -> (Just commit, Cmd.SaveGit repo)
        Right fname -> (Nothing, Cmd.SaveState fname)
    clear entry = entry { Cmd.hist_commit = Nothing }

set_state :: SaveFile -> Bool -> State.State -> Cmd.CmdT IO ()
set_state git_or_state clear_history state = do
    set_save_file git_or_state clear_history
    old_cmd <- Cmd.get
    old_ui <- State.get
    liftIO $ close_state old_cmd old_ui

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
