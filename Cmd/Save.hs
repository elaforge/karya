-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Functions to save and restore state to and from files.

    The naming convention is that @load@ and @save@ functions either load
    the given file, replace the state with it, and set the SaveFile, or save
    the current state and set the SaveFile.  @read@ and @write@ functions
    are lower level and either read the file and return the state, or write the
    given state, without messing with the SaveFile.
-}
module Cmd.Save where
import Prelude hiding (read)
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
    Just (Cmd.SaveRepo repo) -> save_git_as repo
    Just (Cmd.SaveState fn) -> save_state_as fn

-- | Like 'read', but replace the current state and set 'Cmd.state_save_file'.
load :: FilePath -> Cmd.CmdT IO ()
load path = do
    (state, save_file) <- read path
    set_state save_file True state

-- | Try to guess whether the given path is a git save or state save.  If it's
-- a directory, look inside for a .git or .state save.
read :: FilePath -> Cmd.CmdT IO (State.State, SaveFile)
read path
    | SaveGit.is_git path = read_git path Nothing
    | otherwise = ifM (isdir path) look_in_dir (read_state path)
    where
    look_in_dir =
        ifM (isdir git_fn) (read_git git_fn Nothing) $
        ifM (isfile state_fn) (read_state state_fn) $
        ifM (orM [isfile state_fn, isfile (state_fn <> ".gz")])
            (read_state state_fn) $
            Cmd.throw $ "directory contains neither " <> git_fn <> " nor "
                <> state_fn <> " nor " <> state_fn <> ".gz"
        where
        git_fn = path </> default_git
        state_fn = path </> default_state
    isdir = Cmd.rethrow_io . Directory.doesDirectoryExist
    isfile = Cmd.rethrow_io . Directory.doesFileExist


-- * plain serialize

state_magic :: Serialize.Magic
state_magic = Serialize.Magic 's' 'c' 'o' 'r'

save_state :: Cmd.CmdT IO ()
save_state = save_state_as =<< get_state_path

-- | Save the state to the given file and set 'Cmd.state_save_file'.
--
-- The directory of the filename will become the project directory, so things
-- like the saved REPL history and the ly subdirectory will go there.
save_state_as :: FilePath -> Cmd.CmdT IO ()
save_state_as fname = do
    write_current_state fname
    set_save_file (SaveState fname) False

write_current_state :: FilePath -> Cmd.CmdT IO ()
write_current_state fname = do
    state <- State.get
    Log.notice $ "write state to " ++ show fname
    liftIO $ write_state fname state

write_state :: FilePath -> State.State -> IO ()
write_state fname state =
    Serialize.serialize state_magic fname (State.clear state)

load_state :: FilePath -> Cmd.CmdT IO ()
load_state fname = do
    (state, save_file) <- read_state fname
    set_state save_file True state

read_state :: FilePath -> Cmd.CmdT IO (State.State, SaveFile)
read_state fname = do
    let mkmsg = (("load " ++ fname ++ ": ") ++)
    fname <- return $ if FilePath.takeExtension fname == ".gz"
        then FilePath.dropExtension fname else fname
    Log.notice $ "read state from " ++ show fname
    state <- Cmd.require_msg (mkmsg "doesn't exist")
        =<< Cmd.require_right mkmsg =<< liftIO (read_state_ fname)
    return (state, SaveState fname)

-- | Lower level 'read_state'.
read_state_ :: FilePath -> IO (Either String (Maybe State.State))
read_state_ = Serialize.unserialize state_magic

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
    Just (Cmd.SaveRepo repo) -> FilePath.replaceExtension repo ".state"

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
    let rethrow = Cmd.require_right (("save git " <> repo <> ": ") <>)
    commit <- case Cmd.hist_last_commit $ Cmd.state_history_config cmd_state of
        Just commit -> return commit
        Nothing -> do
            state <- State.get
            rethrow =<< liftIO (SaveGit.checkpoint repo
                (SaveGit.SaveHistory state Nothing [] ["save"]))
    save <- rethrow =<< liftIO (SaveGit.set_save_tag repo commit)
    Log.notice $ "wrote save " ++ show save ++ " to " ++ show repo
    set_save_file (SaveRepo repo commit Nothing) False

load_git :: FilePath -> Maybe SaveGit.Commit -> Cmd.CmdT IO ()
load_git repo maybe_commit = do
    (state, save_file) <- read_git repo maybe_commit
    set_state save_file True state

read_git :: FilePath -> Maybe SaveGit.Commit
    -> Cmd.CmdT IO (State.State, SaveFile)
read_git repo maybe_commit = do
    (state, commit, names) <- Cmd.require_right
        (("load git " <> repo <> ": ") <>)
        =<< liftIO (SaveGit.load repo maybe_commit)
    Log.notice $ "read from " <> show repo <> ", at " <> Pretty.pretty commit
        <> " names: " <> show names
    return (state, SaveRepo repo commit (Just names))

-- | Revert to given save point, or the last one.
revert :: Maybe String -> Cmd.CmdT IO ()
revert maybe_ref = do
    save_file <- Cmd.require_msg "can't revert when there is no save file"
        =<< Cmd.gets Cmd.state_save_file
    case save_file of
        Cmd.SaveState fn -> do
            whenJust maybe_ref $ Cmd.throw
                .  ("can't revert to a commit when the save file isn't git: "++)
            load fn
        Cmd.SaveRepo repo -> revert_git repo
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
    Just (Cmd.SaveRepo repo) -> repo

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
save_views :: Cmd.State -> State.State -> IO ()
save_views cmd_state ui_state = case Cmd.state_save_file cmd_state of
    Just (Cmd.SaveRepo repo) ->
        SaveGit.save_views repo $ State.state_views ui_state
    _ -> return ()

-- | This is just like 'Cmd.SaveFile', except SaveRepo has more data.
data SaveFile =
    SaveState !FilePath
    -- | The Strings are the cmd name of this commit, and only set on a git
    -- load.
    | SaveRepo !SaveGit.Repo !SaveGit.Commit !(Maybe [String])
    deriving (Show)

-- | If I switch away from a repo (either to another repo or to a plain state),
-- I have to clear out all the remains of the old repo, since its Commits are
-- no longer valid.
--
-- It's really important to call this whenever you change
-- 'Cmd.state_save_file'!
set_save_file :: SaveFile -> Bool -> Cmd.CmdT IO ()
set_save_file save_file clear_history = do
    cmd_state <- Cmd.get
    when (Just file /= Cmd.state_save_file cmd_state) $ do
        ui_state <- State.get
        liftIO $ save_views cmd_state ui_state
        Cmd.modify $ \state -> state
            { Cmd.state_save_file = Just file
            , Cmd.state_history = let hist = Cmd.state_history state in hist
                { Cmd.hist_past = if clear_history then []
                    else map clear (Cmd.hist_past hist)
                , Cmd.hist_present = (Cmd.hist_present hist)
                    { Cmd.hist_commit = maybe_commit }
                , Cmd.hist_future = []
                }
            , Cmd.state_history_config = (Cmd.state_history_config state)
                { Cmd.hist_last_commit = maybe_commit }
            }
    where
    (maybe_commit, file) = case save_file of
        SaveState fname -> (Nothing, Cmd.SaveState fname)
        SaveRepo repo commit _ -> (Just commit, Cmd.SaveRepo repo)
    clear entry = entry { Cmd.hist_commit = Nothing }

set_state :: SaveFile -> Bool -> State.State -> Cmd.CmdT IO ()
set_state save_file clear_history state = do
    set_save_file save_file clear_history
    Play.cmd_stop
    Cmd.modify $ Cmd.reinit_state (Cmd.empty_history_entry state)
    -- Names is only set on a git load.  This will cause "Cmd.Undo" to clear
    -- out the history.
    case save_file of
        SaveRepo _ commit (Just names) -> Cmd.modify $ \st -> st
            { Cmd.state_history = (Cmd.state_history st)
                { Cmd.hist_last_cmd = Just $ Cmd.Load (Just commit) names }
            }
        _ -> return ()
    State.put (State.clear state)
    root <- case State.config_root (State.state_config state) of
        Nothing -> return Nothing
        Just root -> Seq.head . Map.keys <$> State.views_of root
    let focused = msum [root, Seq.head (Map.keys (State.state_views state))]
    whenJust focused ViewConfig.bring_to_front

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
