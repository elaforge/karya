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
module Cmd.Save (
    -- * universal
    save, load, read, load_template
    , infer_save_type
    -- * state
    , save_state, save_state_as, load_state
    , read_state, read_state_, write_state
    , write_current_state
    , get_state_path
    -- * git
    , save_git, save_git_as, load_git, revert
    , get_git_path
    -- * config
    , save_midi_config, load_midi_config
    -- * misc
    , save_views
) where
import Prelude hiding (read)
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.Locale as Locale

import qualified Util.Git as Git
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.Serialize as Serialize
import qualified Cmd.ViewConfig as ViewConfig

import qualified App.Config as Config
import Global


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
read path = do
    path <- expand_filename path
    save <- Cmd.require_right ("read: "<>) =<< liftIO (infer_save_type path)
    case save of
        Cmd.SaveRepo repo -> read_git repo Nothing
        Cmd.SaveState fn -> read_state fn

-- | Like 'load', but set the name to \"untitled\" so you don't overwrite it
-- when you save.
load_template :: FilePath -> Cmd.CmdT IO ()
load_template fn = do
    (state, _) <- read fn
    let save_file = SaveState $ FilePath.takeDirectory fn </> "untitled"
    set_state save_file True state
    now <- liftIO $ Time.getCurrentTime
    State.put $
        State.config#State.meta#State.creation #= now $
        State.clear state

-- | Given a path, which is either a file or a directory, try to figure out
-- what to load.  Saves can be either a plain saved state, or a directory
-- containing either a git repo @save.git@, or a state @save.state@.  If
-- both are present, the git repo is preferred.
infer_save_type :: FilePath -> IO (Either String Cmd.SaveFile)
infer_save_type path = fmap prepend $ cond
    [ (return $ SaveGit.is_git path, ok $ Cmd.SaveRepo path)
    , (is_dir path, cond
        [ (is_dir git_fn, ok $ Cmd.SaveRepo git_fn)
        , (is_file state_fn, ok $ Cmd.SaveState state_fn)
        ] $ return $ Left $ "directory contains neither " <> git_fn <> " nor "
            <> state_fn)
    , (is_file path, ok $ Cmd.SaveState path)
    ] $ return $ Left "file not found"
    where
    prepend (Left err) = Left $ path <> ": " <> err
    prepend (Right val) = Right val
    ok = return . Right
    git_fn = path </> default_git
    state_fn = path </> default_state
    is_dir = Directory.doesDirectoryExist
    is_file = Directory.doesFileExist

-- | Like guard cases but with monadic conditions.
cond :: Monad m => [(m Bool, m a)] -> m a -> m a
cond [] consequent = consequent
cond ((condition, result) : rest) consequent =
    ifM condition result (cond rest consequent)

-- * expand path

-- | Expand `-delimited macros to make a filepath.
expand_filename :: FilePath -> Cmd.CmdT IO FilePath
expand_filename =
    fmap untxt . TextUtil.mapDelimitedM False '`' expand . txt
    where
    expand text
        | text == "y-m-d" = liftIO date
        | text == "d" = do
            dir <- Cmd.require "`d` requires a save dir"
                =<< Cmd.gets Cmd.state_save_dir
            return $ txt dir
        | otherwise = return $ "`" <> text <> "`"

date :: IO Text
date = do
    tz <- Time.getCurrentTimeZone
    today <- Time.utcToLocalTime tz <$> Time.getCurrentTime
    return $ txt $ Time.formatTime Locale.defaultTimeLocale "%y-%m-%d" today

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
    fname <- write_current_state fname
    set_save_file (SaveState fname) False

write_current_state :: FilePath -> Cmd.CmdT IO FilePath
write_current_state fname = do
    fname <- expand_filename fname
    state <- State.get
    ((), secs) <- liftIO $ Log.time_eval $ write_state fname state
    Log.notice $ "wrote state to " <> showt fname
        <> ", took " <> prettyt secs <> "s"
    return fname

write_state :: FilePath -> State.State -> IO ()
write_state fname state = do
    now <- Time.getCurrentTime
    Serialize.serialize state_magic fname $
        State.config#State.meta#State.last_save #= now $
        State.clear state

load_state :: FilePath -> Cmd.CmdT IO ()
load_state fname = do
    (state, save_file) <- read_state fname
    set_state save_file True state

read_state :: FilePath -> Cmd.CmdT IO (State.State, SaveFile)
read_state fname = do
    let mkmsg = (("load " <> fname <> ": ") <>)
    Log.notice $ "read state from " <> showt fname
    state <- Cmd.require (mkmsg "doesn't exist")
        =<< Cmd.require_right mkmsg =<< liftIO (read_state_ fname)
    return (state, SaveState fname)

-- | Lower level 'read_state'.
read_state_ :: FilePath -> IO (Either String (Maybe State.State))
read_state_ = Serialize.unserialize state_magic

-- ** path

get_state_path :: Cmd.M m => m FilePath
get_state_path = do
    ns <- State.get_namespace
    state <- Cmd.get
    return $ make_state_path ns state

make_state_path :: Id.Namespace -> Cmd.State -> FilePath
make_state_path ns state = case Cmd.state_save_file state of
    Nothing -> Cmd.path state Config.save_dir </> untxt (Id.un_namespace ns)
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
    repo <- expand_filename repo
    cmd_state <- Cmd.get
    let rethrow = Cmd.require_right (("save git " <> repo <> ": ") <>)
    commit <- case Cmd.hist_last_commit $ Cmd.state_history_config cmd_state of
        Just commit -> return commit
        Nothing -> do
            state <- State.get
            rethrow =<< liftIO (SaveGit.checkpoint repo
                (SaveGit.SaveHistory state Nothing [] ["save"]))
    save <- rethrow =<< liftIO (SaveGit.set_save_tag repo commit)
    Log.notice $ "wrote save " <> showt save <> " to " <> showt repo
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
    Log.notice $ "read from " <> showt repo <> ", at " <> prettyt commit
        <> " names: " <> showt names
    return (state, SaveRepo repo commit (Just names))

-- | Revert to given save point, or the last one.
revert :: Maybe String -> Cmd.CmdT IO ()
revert maybe_ref = do
    save_file <- Cmd.require "can't revert when there is no save file"
        =<< Cmd.gets Cmd.state_save_file
    case save_file of
        Cmd.SaveState fn -> do
            whenJust maybe_ref $ Cmd.throw
                .  ("can't revert to a commit when the save file isn't git: "++)
            load fn
        Cmd.SaveRepo repo -> revert_git repo
    Log.notice $ "revert to " <> showt save_file
    where
    revert_git repo = do
        save <- case maybe_ref of
            Nothing -> fmap fst $ Cmd.require "no last save"
                =<< liftIO (SaveGit.read_last_save repo Nothing)
            Just ref -> Cmd.require ("unparseable SavePoint: " ++ show ref)
                (SaveGit.ref_to_save ref)
        commit <- Cmd.require ("save ref not found: " ++ show save)
            =<< rethrow "revert" (SaveGit.read_save_ref repo save)
        load_git repo (Just commit)

rethrow :: String -> IO a -> Cmd.CmdT IO a
rethrow caller io =
    Cmd.require_right id =<< liftIO (SaveGit.try caller io)

-- ** path

get_git_path :: Cmd.M m => m Git.Repo
get_git_path = do
    ns <- State.get_namespace
    state <- Cmd.get
    return $ make_git_path ns state

make_git_path :: Id.Namespace -> Cmd.State -> Git.Repo
make_git_path ns state = case Cmd.state_save_file state of
    Nothing -> Cmd.path state Config.save_dir </> untxt (Id.un_namespace ns)
        </> default_git
    Just (Cmd.SaveState fn) -> FilePath.replaceExtension fn ".git"
    Just (Cmd.SaveRepo repo) -> repo

default_git :: FilePath
default_git = "save.git"

-- * config

save_midi_config :: FilePath -> Cmd.CmdT IO ()
save_midi_config fname = do
    config <- State.get_config id
    Log.notice $ "write midi config to " <> showt fname
    liftIO $ Serialize.serialize_pretty_text fname
        (State.config_midi config, State.config_aliases config)

load_midi_config :: FilePath -> Cmd.CmdT IO ()
load_midi_config fname = do
    Log.notice $ "load midi config from " <> showt fname
    (config, aliases) <- Cmd.require_right
        (("unserializing midi config " ++ show fname ++ ": ") ++)
        =<< liftIO (Serialize.unserialize_text fname)
    State.modify_config $ (State.midi #= config) . (State.aliases #= aliases)

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
    | SaveRepo !SaveGit.Repo !SaveGit.Commit !(Maybe [Text])
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
