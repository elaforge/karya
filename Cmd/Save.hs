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
    -- * quit
    softQuit, hardQuit
    -- * universal
    , save, saveAs, load, loadForce, read, read_, loadTemplate
    , inferSaveType
    -- * state
    , saveState, saveStateAs
    , readState, readState_, writeStateDir
    , writeCurrentState
    -- ** path
    , getStatePath, statePathForRepo
    -- * git
    , saveGit, saveGitAs, loadGit, revert
    , getGitPath
    -- * misc
    , saveViews
) where
import           Prelude hiding (read)
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Files as Files
import qualified Util.Git as Git
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Serialize as Serialize
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Play as Play
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.SaveGitT as SaveGitT
import qualified Cmd.Serialize

import qualified Ui.Id as Id
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global


-- * quit

-- | Warn and abort if there is unsaved data, otherwise quit.
softQuit :: Cmd.CmdT IO Cmd.Status
softQuit = checkUnsavedData >>= \case
    Just msg -> do
        Log.warn $ "refusing to quit: " <> msg
        pure Cmd.Done
    Nothing -> hardQuit

checkUnsavedData :: Cmd.M m => m (Maybe Text)
checkUnsavedData = Cmd.gets Cmd.stateSaved >>= \case
    Cmd.Saved Cmd.UnsavedChanges _ -> pure $ Just "unsaved changes"
    Cmd.Saved _ True ->
        pure $ Just "repl editor is open, possibly with unsaved changes"
    _ -> pure Nothing

-- | Quit.  If there is unsaved data, it will be discarded!
hardQuit :: Cmd.CmdT IO Cmd.Status
hardQuit = Play.cmdStop >> pure Cmd.Quit

-- * universal

-- | Save to the current 'Cmd.state_save_file', or create a new git repo if
-- there is none.
save :: Cmd.CmdT IO ()
save = Cmd.gets Cmd.stateSaveFile >>= \case
    Nothing -> saveGit
    -- Try to override Cmd.Writable on an explicit save.  If it's still
    -- read only, this should throw an exception.
    Just (_, Cmd.SaveRepo repo) -> saveGitAs (Path.toPath repo)
    Just (_, Cmd.SaveState fn) -> saveStateAs (Path.toPath fn)

saveAs :: FilePath -> Cmd.CmdT IO ()
saveAs path = Cmd.gets Cmd.stateSaveFile >>= \case
    Nothing -> saveGitAs path
    Just (_, Cmd.SaveRepo _) -> saveGitAs path
    Just (_, Cmd.SaveState _) -> saveStateAs path

-- | Like 'read', but replace the current state and set 'Cmd.state_save_file'.
load :: FilePath -> Cmd.CmdT IO ()
load path = checkUnsavedData >>= \case
    Just msg -> Log.warn $ "refusing to load a new score: " <> msg
    Nothing -> loadForce path

loadForce :: FilePath -> Cmd.CmdT IO ()
loadForce path = do
    (state, saveFile) <- read path
    setLoadedState saveFile state

-- | Try to guess whether the given path is a git save or state save.  If it's
-- a directory, look inside for a .git or .state save.
read :: FilePath -> Cmd.CmdT IO (Ui.State, StateSaveFile)
read path = do
    path <- expandFilename path
    save <- Cmd.requireRight ("read: "<>) =<< liftIO (inferSaveType path)
    case save of
        Cmd.SaveRepo repo -> readGit (Path.toPath repo) Nothing
        Cmd.SaveState fn -> readState (Path.toPath fn)

-- | Low level 'read'.
read_ :: FilePath -> IO (Either Text Ui.State)
read_ path = inferSaveType path >>= \case
    Left err -> pure $ Left $ "read " <> showt path <> ": " <> err
    Right save -> case save of
        Cmd.SaveState fname -> first pretty <$> readState_ (Path.toPath fname)
        Cmd.SaveRepo repo ->
            fmap extract <$> readGit_ (Path.toPath repo) Nothing
            where extract (state, _, _) = state

-- | Like 'load', but don't set SaveFile, so you can't overwrite the loaded
-- file when you save.
loadTemplate :: FilePath -> Cmd.CmdT IO ()
loadTemplate fn = do
    (state, _) <- read fn
    setLoadedState Nothing state
    now <- liftIO $ Time.getCurrentTime
    Ui.modifyConfig $ UiConfig.meta#UiConfig.creation #= now

-- | Given a path, which is either a file or a directory, try to figure out
-- what to load.  Saves can be either a plain saved state, or a directory
-- containing either a git repo @save.git@, or a state @save.state@.  If
-- both are present, the git repo is preferred.
inferSaveType :: FilePath -> IO (Either Text Cmd.SaveFile)
inferSaveType path = fmap prepend $ condM
    [ (pure $ SaveGit.isGit path,
        Right . Cmd.SaveRepo <$> Path.canonical path)
    , (isDir path, condM
        [ (isDir gitFn, Right . Cmd.SaveRepo <$> Path.canonical gitFn)
        , (isFile stateFn, Right . Cmd.SaveState <$> Path.canonical stateFn)
        ] $ pure $ Left $ "directory contains neither " <> txt gitFn
            <> " nor " <> txt stateFn)
    , (isFile path, Right . Cmd.SaveState <$> Path.canonical path)
    ] $ pure $ Left "file not found"
    where
    prepend (Left err) = Left $ txt path <> ": " <> err
    prepend (Right val) = Right val
    gitFn = path </> defaultGit
    stateFn = path </> defaultState
    isDir = Directory.doesDirectoryExist
    isFile = Directory.doesFileExist

-- | Like guard cases but with monadic conditions.
condM :: Monad m => [(m Bool, m a)] -> m a -> m a
condM [] consequent = consequent
condM ((condition, result) : rest) consequent =
    ifM condition result (condM rest consequent)

-- * expand path

-- | Expand `-delimited macros to make a FilePath.  Also make it relative to
-- Config.save_dir if it isn't already.
--
-- TODO: These are relative to app_dir but I don't use Path.Relative yet.
expandFilename :: FilePath -> Cmd.CmdT IO FilePath
expandFilename =
    fmap (prefix . untxt) . Texts.mapDelimitedM False '`' expand . txt
    where
    expand text = case lookup text filenameMacros of
        Just get -> get
        Nothing -> Cmd.throw $ "unknown macro " <> showt text
            <> ", known macros are: "
            <> Text.intercalate ", " (map fst filenameMacros)
    prefix path
        -- I have the habit of putting on "save/" manually.
        | path `Path.inDir` Path.unrelative Config.saveDir = path
        | path `Path.inDir` "save" = path
        | otherwise = Path.unrelative Config.saveDir </> path

filenameMacros :: [(Text, Cmd.CmdT IO Text)]
filenameMacros =
    [ ("y-m-d", liftIO date)
    , ("d", fmap txt $ Cmd.require "`d` requires a save dir"
        =<< Cmd.gets Cmd.stateSaveDir)
    ]

date :: IO Text
date = do
    tz <- Time.getCurrentTimeZone
    today <- Time.utcToLocalTime tz <$> Time.getCurrentTime
    pure $ txt $ Time.formatTime Time.defaultTimeLocale "%y-%m-%d" today

-- * plain serialize

saveState :: Cmd.CmdT IO ()
saveState = saveStateAs =<< Cmd.require "can't save, no save file"
    =<< getStatePath

-- | Save the state to the given file and set 'Cmd.state_save_file'.
--
-- The directory of the filename will become the project directory, so things
-- like the saved REPL history and the ly subdirectory will go there.
saveStateAs :: FilePath -> Cmd.CmdT IO ()
saveStateAs fname = do
    fname <- writeCurrentState fname
    copyKy (Cmd.kyPath fname)
    setSaveFile (Just (Cmd.ReadWrite, SaveState fname)) Save

writeCurrentState :: FilePath -> Cmd.CmdT IO FilePath
writeCurrentState fname = do
    fname <- expandFilename fname
    state <- Ui.get
    (fname, metric) <- rethrowIo "write_current_state" $
        Thread.timeAction $ writeStateDir fname state
    Log.notice $ "wrote state to " <> showt fname
        <> " " <> Transform.shortStats state
        <> ", took " <> Thread.showMetric metric
    pure fname

writeStateDir :: FilePath -> Ui.State -> IO FilePath
writeStateDir path state =
    ifM (Directory.doesFileExist path) (writeState path state >> pure path) $ do
    unlessM (Directory.doesDirectoryExist path) $
        Directory.createDirectory path
    writeState (path </> defaultState) state
    pure $ path </> defaultState

writeState :: FilePath -> Ui.State -> IO ()
writeState fname state = do
    now <- Time.getCurrentTime
    void $ Serialize.serialize Cmd.Serialize.scoreMagic fname $
        Ui.config#UiConfig.meta#UiConfig.lastSave #= now $
        Ui.clear state
    writeKy (Cmd.kyPath fname) $ UiConfig.configKy (Ui.stateConfig state)

-- | New versions keep ky in a separate file, not in UiConfig.  So if this is
-- an old score with internal ky, write it to the external file.
writeKy :: FilePath -> Text -> IO ()
writeKy kyPath ky =
    unlessM (Directory.doesFileExist kyPath) $ unless (ky == "") $
        Text.IO.writeFile kyPath $ Text.strip ky <> "\n"

-- | If I'm saving under a new name, copy the current external ky file to go
-- with it.  This has to be called before 'setSaveFile', while
-- 'Cmd.stateSaveFile' still has the old name.
copyKy :: FilePath -> Cmd.CmdT IO ()
copyKy newKy = do
    mbOldKy <- Cmd.gets $ fmap (Cmd.saveFileKyPath . snd) . Cmd.stateSaveFile
    whenJust mbOldKy $ \oldKy -> liftIO $
        whenM (Directory.doesFileExist oldKy) $ do
            newKy <- Directory.canonicalizePath newKy
            unless (oldKy == newKy) $ Directory.copyFile oldKy newKy

-- loadStateDir :: FilePath -> Cmd.CmdT IO ()
-- loadStateDir path =
--     ifM (liftIO $ Directory.doesFileExist (path </> defaultState))
--         (loadState (path </> defaultState)) (loadState path)
--
-- loadState :: FilePath -> Cmd.CmdT IO ()
-- loadState fname = do
--     (state, saveFile) <- readState fname
--     setLoadedState saveFile state

readState :: FilePath -> Cmd.CmdT IO (Ui.State, StateSaveFile)
readState fname = do
    let mkmsg err = "load " <> txt fname <> ": " <> pretty err
    writable <- liftIO $ Files.writable fname
    (state, metric) <- Thread.timeAction $
        Cmd.requireRight mkmsg =<< liftIO (readState_ fname)
    Log.notice $ "read state from " <> showt fname
        <> (if writable then " " else " (ro) ")
        <> Transform.shortStats state
        <> ", took " <> Thread.showMetric metric
    pure (state, Just
        (if writable then Cmd.ReadWrite else Cmd.ReadOnly, SaveState fname))

-- | Low level 'readState'.
readState_ :: FilePath
    -> IO (Either Serialize.UnserializeError Ui.State)
readState_ = Serialize.unserialize Cmd.Serialize.scoreMagic

-- | Low level 'readGit'.
readGit_ :: SaveGit.Repo -> Maybe SaveGit.Commit
    -> IO (Either Text (Ui.State, SaveGit.Commit, [Text]))
readGit_ = SaveGit.load


-- ** path

getStatePath :: Cmd.M m => m (Maybe FilePath)
getStatePath = do
    state <- Cmd.get
    pure $ makeStatePath . snd <$> Cmd.stateSaveFile state

makeStatePath :: Cmd.SaveFile -> FilePath
makeStatePath (Cmd.SaveState fn) = Path.toPath fn
makeStatePath (Cmd.SaveRepo repo) = statePathForRepo (Path.toPath repo)

-- | Get a state save path based on a repo path.  This is for saving a backup
-- state, or when switching from SaveRepo to SaveState.
statePathForRepo :: SaveGit.Repo -> FilePath
statePathForRepo repo = FilePath.replaceExtension repo ".state"

defaultState :: FilePath
defaultState = "save.state"

-- * git serialize

-- | Save a SavePoint to the git repo in 'Cmd.state_save_file', or start a new
-- one.  Set the 'Cmd.state_save_file' to the repo, so I'll keep saving to
-- that repo.
saveGit :: Cmd.CmdT IO ()
saveGit = saveGitAs =<< getGitPath

saveGitAs :: SaveGit.Repo -- ^ Save to this repo, or create it.
    -- 'Cmd.Undo.maintainHistory' will start checkpointing to it.
    -- @.git@ is appended if it doesn't already have that suffix.
    -> Cmd.CmdT IO ()
saveGitAs repo = do
    repo <- expandFilename repo
    repo <- pure $ if SaveGit.gitSuffix `List.isSuffixOf` repo then repo
        else repo ++ SaveGit.gitSuffix
    cmdState <- Cmd.get
    let rethrow = Cmd.requireRight (("save git " <> txt repo <> ": ") <>)
    commit <- case Cmd.histLastCommit $ Cmd.stateHistoryConfig cmdState of
        Just commit -> pure commit
        Nothing -> do
            let user = Cmd.configGitUser $ Cmd.stateConfig cmdState
            state <- Ui.get
            rethrow =<< liftIO (SaveGit.checkpoint user repo
                (SaveGitT.SaveHistory state Nothing [] ["save"]))
    save <- rethrow =<< liftIO (SaveGit.setSaveTag repo commit)
    uiState <- Ui.get
    liftIO $ writeKy (Cmd.repoKyPath repo) $
        UiConfig.configKy (Ui.stateConfig uiState)
    copyKy (Cmd.repoKyPath repo)
    Log.notice $ "wrote save " <> showt save <> " to " <> showt repo
    setSaveFile (Just (Cmd.ReadWrite, SaveRepo repo commit Nothing)) Save

loadGit :: FilePath -> Maybe SaveGit.Commit -> Cmd.CmdT IO ()
loadGit repo maybeCommit = do
    (state, saveFile) <- readGit repo maybeCommit
    setLoadedState saveFile state

readGit :: FilePath -> Maybe SaveGit.Commit
    -> Cmd.CmdT IO (Ui.State, StateSaveFile)
readGit repo maybeCommit = do
    (state, commit, names) <- Cmd.requireRight
        (("load git " <> txt repo <> ": ") <>)
        =<< liftIO (readGit_ repo maybeCommit)
    writable <- liftIO $ Files.writable repo
    Log.notice $ "read from " <> showt repo <> ", at " <> pretty commit
        <> " names: " <> showt names
        <> if writable then "" else " (read-only, not setting save file)"
    pure (state, Just (if writable then Cmd.ReadWrite else Cmd.ReadOnly,
        SaveRepo repo commit (Just names)))

-- | Revert to given save point, or the last one.
revert :: Maybe String -> Cmd.CmdT IO ()
revert maybeRef = do
    saveFile <- Cmd.require "can't revert when there is no save file"
        =<< Cmd.gets Cmd.stateSaveFile
    case saveFile of
        (_, Cmd.SaveState fn) -> do
            whenJust maybeRef $ \ref -> Cmd.throw $
                "can't revert to a commit when the save file isn't git: "
                <> txt ref
            load (Path.toPath fn)
        (_, Cmd.SaveRepo repo) -> revertGit (Path.toPath repo)
    Log.notice $ "revert to " <> showt saveFile
    where
    revertGit repo = do
        save <- case maybeRef of
            Nothing -> fmap fst $ Cmd.require "no last save"
                =<< liftIO (SaveGit.readLastSave repo Nothing)
            Just ref -> Cmd.require ("unparseable SavePoint: " <> showt ref)
                (SaveGit.refToSave ref)
        commit <- Cmd.require ("save ref not found: " <> showt save)
            =<< rethrowGit "revert" (SaveGit.readSaveRef repo save)
        loadGit repo (Just commit)

rethrowGit :: Text -> IO a -> Cmd.CmdT IO a
rethrowGit caller io = Cmd.requireRight id =<< liftIO (SaveGit.try caller io)

rethrowIo :: Text -> IO a -> Cmd.CmdT IO a
rethrowIo caller io = Cmd.requireRight
    (\exc -> caller <> ": " <> showt (exc :: Exception.IOException))
        =<< liftIO (Exception.try io)

-- ** path

getGitPath :: Cmd.M m => m Git.Repo
getGitPath = do
    ns <- Ui.getNamespace
    state <- Cmd.get
    pure $ makeGitPath ns state

makeGitPath :: Id.Namespace -> Cmd.State -> Git.Repo
makeGitPath ns state = case Cmd.stateSaveFile state of
    Nothing -> Cmd.toAbsolute state Config.saveDir
        </> untxt (Id.unNamespace ns) </> defaultGit
    Just (_, Cmd.SaveState fn) ->
        FilePath.replaceExtension (Path.toPath fn) SaveGit.gitSuffix
    Just (_, Cmd.SaveRepo repo) -> Path.toPath repo

defaultGit :: FilePath
defaultGit = "save" <> SaveGit.gitSuffix

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
saveViews :: Cmd.State -> Ui.State -> IO ()
saveViews cmdState uiState = case Cmd.stateSaveFile cmdState of
    Just (Cmd.ReadWrite, Cmd.SaveRepo repo) ->
        SaveGit.saveViews (Path.toPath repo) $ Ui.stateViews uiState
    _ -> pure ()

-- | This is just like 'Cmd.SaveFile', except SaveRepo has more data.
data SaveFile
    = SaveState !FilePath
    -- | The Strings are the cmd name of this commit, and only set on a git
    -- load.
    | SaveRepo !SaveGit.Repo !SaveGit.Commit !(Maybe [Text])
    deriving (Show)
type StateSaveFile = Maybe (Cmd.Writable, SaveFile)

data SaveDirection = Save | Load deriving (Show)

-- | Do the necessary housekeeping after loading a new Ui.State.
setLoadedState :: StateSaveFile -> Ui.State -> Cmd.CmdT IO ()
setLoadedState saveFile state = do
    setSaveFile saveFile Load
    Play.cmdStop
    Cmd.modify $ Cmd.reinitState (Cmd.emptyHistoryEntry state)
    -- Names is only set on a git load.  This will cause "Cmd.Undo" to clear
    -- out the history.
    case saveFile of
        Just (_, SaveRepo _ commit (Just names)) -> Cmd.modify $ \st -> st
            { Cmd.stateHistory = (Cmd.stateHistory st)
                { Cmd.histLastCmd = Just $ Cmd.Load (Just commit) names }
            }
        _ -> pure ()
    old <- Ui.get
    Ui.put $ Ui.clear $
        Transform.replaceNamespace Clip.clipNamespace old state
    root <- case UiConfig.configRoot (Ui.stateConfig state) of
        Nothing -> pure Nothing
        Just root -> Lists.head . Map.keys <$> Ui.viewsOf root
    -- Try to focus on the root block, for consistency.
    let focused = msum [root, Lists.head $ Map.keys (Ui.stateViews state)]
    whenJust focused Cmd.focus

-- | If I switch away from a repo (either to another repo or to a plain state),
-- I have to clear out all the remains of the old repo, since its Commits are
-- no longer valid.
--
-- It's really important to call this whenever you change
-- 'Cmd.state_save_file'!
setSaveFile :: StateSaveFile -> SaveDirection -> Cmd.CmdT IO ()
setSaveFile saveFile direction = do
    (maybeCommit, file) <- case saveFile of
        Nothing -> pure (Nothing, Nothing)
        Just (writable, save) -> case save of
            SaveState fname -> do
                path <- liftIO $ Path.canonical fname
                pure (Nothing, Just (writable, Cmd.SaveState path))
            SaveRepo repo commit _ -> do
                path <- liftIO $ Path.canonical repo
                pure (Just commit, Just (writable, Cmd.SaveRepo path))
    cmdState <- Cmd.get
    when (file /= Cmd.stateSaveFile cmdState) $ do
        uiState <- Ui.get
        liftIO $ saveViews cmdState uiState
        Cmd.modify $ \state -> state
            { Cmd.stateSaveFile = file
            , Cmd.stateHistory = let hist = Cmd.stateHistory state in hist
                { Cmd.histPast = case direction of
                    Load -> []
                    Save -> map clear (Cmd.histPast hist)
                , Cmd.histPresent = (Cmd.histPresent hist)
                    { Cmd.histCommit = maybeCommit }
                , Cmd.histFuture = []
                }
            , Cmd.stateHistoryConfig = (Cmd.stateHistoryConfig state)
                { Cmd.histLastCommit = maybeCommit }
            }
        -- This is not needed for the MIDI backend, but if there is im cache,
        -- I need to trigger it to regenerate under the new name.
        Cmd.invalidatePerformances
    -- This is called both when saving and loading, so it's a good place to
    -- mark that the state is synced to disk.
    Cmd.modify $ \st -> st
        { Cmd.stateSaved = (Cmd.stateSaved st)
            { Cmd._savedState = case direction of
                Load -> Cmd.JustLoaded
                Save -> Cmd.SavedChanges
            }
        }
    where
    clear entry = entry { Cmd.histCommit = Nothing }
