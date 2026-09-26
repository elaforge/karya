-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Cmd.SaveGit (
    SaveHistory(..), LoadHistory(..), Repo, Commit
    -- * save point
    , isGit, gitSuffix
    , SavePoint, setSaveTag, writeSaveRef, readSaveRef
    , readLastSave, refToSave
    -- * save
    , checkpoint, save, shouldRecord
    -- * load
    , load, loadPreviousHistory, loadNextHistory
    -- * views
    , saveViews, loadViews
    -- * util
    , inferCommit, try
    -- * User
    , getUser
#ifdef TESTING
    , parseNames, loadFrom
#endif
) where
import qualified Control.Exception as Exception
import           Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified GHC.Float as Float
import qualified Numeric
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process

import qualified Util.Files as Files
import qualified Util.Git as Git
import           Util.GitT (Commit, Repo)
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Serialize as Serialize

import qualified Cmd.SaveGitT as SaveGitT
import           Cmd.SaveGitT (SaveHistory(..))
import qualified Cmd.Serialize

import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import           Global
import           Types


-- | History loaded from disk.  It only has CmdUpdates so you can feed them to
-- diff.
data LoadHistory = LoadHistory !Ui.State !Commit !Update.UiDamage ![Text]
    deriving (Show)

isGit :: FilePath -> Bool
isGit = (gitSuffix `List.isSuffixOf`)

gitSuffix :: FilePath
gitSuffix = ".git"

-- * save point

-- | Add a new save point tag to the given commit, unless it already has one.
setSaveTag :: Git.Repo -> Commit -> IO (Either Text SavePoint)
setSaveTag repo commit = try "set_save_tag" $ do
    Git.gc repo -- Might as well clean things up at this point.
    readLastSave repo (Just commit) >>= \case
        Nothing -> do
            save <- findNextSave repo (SavePoint [])
            writeSaveRef repo save commit
            pure save
        Just (lastSave, saveCommit) -> do
            save <- findNextSave repo lastSave
            if commit == saveCommit then pure lastSave
                else writeSaveRef repo save commit >> pure save

-- | Stored in reverse order as in the ref name.
newtype SavePoint = SavePoint [Int] deriving (Eq, Show, Pretty)

-- | Create a tag for the given commit.
writeSaveRef :: Git.Repo -> SavePoint -> Commit -> IO ()
writeSaveRef repo save commit = Git.writeRef repo commit (saveToRef save)

readSaveRef :: Git.Repo -> SavePoint -> IO (Maybe Commit)
readSaveRef repo save = Git.readRef repo (saveToRef save)

readLastSave :: Git.Repo -> Maybe Commit
    -- ^ Find the last save from this commit, or HEAD if not given.
    -> IO (Maybe (SavePoint, Commit))
readLastSave repo maybeCommit = do
    -- This may be called on incomplete repos without a HEAD.
    maybeCommits <- catch $
        maybe (Git.readLogHead repo) (Git.readLogFrom repo) maybeCommit
    let commits = fromMaybe [] maybeCommits
    refs <- Git.readRefMap repo
    let commitToSave = Map.fromList $ do
            (ref, commit) <- Map.toList refs
            Just save <- pure $ refToSave ref
            pure (commit, save)
    pure $ Lists.head [(save, commit) | (Just save, commit)
        <- zip (map (`Map.lookup` commitToSave) commits) commits]

-- | Find the SavePoint that should be added after the given SavePoint.
findNextSave :: Git.Repo -> SavePoint -> IO SavePoint
findNextSave repo save =
    fromJust =<< findM saveFree (iterate split (increment save))
    where
    saveFree save = Maybe.isNothing <$> Git.readRef repo (saveToRef save)
    fromJust = maybe -- This should never happen since iterate tries forever.
        (Git.throw $ "couldn't find a free save name after " ++ show save)
        pure
    split (SavePoint xs) = SavePoint (0 : xs)
    increment (SavePoint []) = SavePoint [0]
    increment (SavePoint (x:xs)) = SavePoint (x + 1 : xs)

refToSave :: Git.Ref -> Maybe SavePoint
refToSave ref
    | not (all (all Char.isDigit) versions) = Nothing
    | otherwise = Just $ SavePoint (reverse (map read versions))
    where
    (save, _) = Lists.dropPrefix "tags/" ref
    versions = Lists.split (=='.') save

saveToRef :: SavePoint -> Git.Ref
saveToRef (SavePoint versions) =
    "tags" </> Lists.join "." (map show (reverse versions))


-- * save

-- | Checkpoint the given SaveHistory.  If it has no previous commit, create
-- a new repo.
checkpoint :: SaveGitT.User -> Git.Repo -> SaveHistory
    -> IO (Either Text Commit)
checkpoint user repo (SaveHistory state Nothing _ names) =
    try "save" $ save user repo state names
checkpoint user repo (SaveHistory state (Just commit) updates names) =
        try "checkpoint" $ do
    let (notFound, mods) = dumpDiff False state (filter shouldRecord updates)
    unless (null notFound) $
        Log.warn $ "ignored updates for nonexistent "
            <> Text.intercalate ", " notFound
            <> "; this probably means 'Ui.Diff.cancel_updates didn't do its job"
    if null mods then pure commit else do
        unlessM (Files.writable repo) $
            Git.throw $ "git repo is not writable: " <> show repo
        lastTree <- Git.commitTree <$> Git.readCommit repo commit
        tree <- Git.modifyTree repo lastTree mods
        commitTree user repo tree (Just commit)
            (unparseNames "checkpoint" names)

-- | Create a new repo, or throw if it already exists.
save :: SaveGitT.User -> Git.Repo -> Ui.State -> [Text] -> IO Commit
save user repo state cmdNames = do
    whenM (Git.init repo) $
        Git.throw "refusing to overwrite a repo that already exists"
    dir <- either (Git.throw . ("make_dir: "<>)) pure $
        Git.makeDir (dump state)
    tree <- Git.writeDir repo dir
    commitTree user repo tree Nothing (unparseNames "save" cmdNames)

-- | True if this update is interesting enough to record a checkpoint for.
shouldRecord :: Update.UiUpdate -> Bool
shouldRecord = \case
    -- BlockConfig changes are only box colors, which I never need to save.
    Update.Block _ (Update.BlockConfig {}) -> False
    update -> not $ Update.isViewUpdate update

commitTree :: SaveGitT.User -> Git.Repo -> Git.Tree -> Maybe Commit -> String
    -> IO Commit
commitTree user repo tree maybeParent desc = do
    commit <- Git.writeCommit repo (SaveGitT.name user) (SaveGitT.email user)
        (maybe [] (:[]) maybeParent) tree desc
    Git.updateHead repo commit
    pure commit

-- ** events update

-- | TODO disabled event because loading isn't implemented.  Should probably
-- just delete it all.
dumpEvents :: Ui.State -> TrackId -> ScoreTime -> ScoreTime
    -> Git.Modification
dumpEvents state trackId start end =
    Git.Add (eventsPath trackId start end) $
        Serialize.encode $ EventsUpdate trackId start end events
    where
    events = maybe Events.empty
        (Events.inRange (Events.Range start end) . Track.trackEvents)
        (Map.lookup trackId (Ui.stateTracks state))

-- | Put the range into the filename.  You still have to load all the event
-- files in the directory, but at least exactly matching ranges will overwrite
-- each other.
eventsPath :: TrackId -> ScoreTime -> ScoreTime -> String
eventsPath trackId start end = idToPath trackId ++ "_"
    </> scoreToHex start ++ "-" ++ scoreToHex end

data EventsUpdate = EventsUpdate TrackId ScoreTime ScoreTime Events.Events
    deriving (Show)

instance Serialize.Serialize EventsUpdate where
    put (EventsUpdate a b c d) = Serialize.put a >> Serialize.put b
        >> Serialize.put c >> Serialize.put d
    get = EventsUpdate <$> Serialize.get <*> Serialize.get <*> Serialize.get
        <*> Serialize.get

scoreToHex :: ScoreTime -> String
scoreToHex = pad . flip Numeric.showHex "" . Float.castDoubleToWord64
    . ScoreTime.toDouble
    where pad s = replicate (16 - length s) '0' ++ s


-- * load

load :: Git.Repo -> Maybe Commit
    -> IO (Either Text (Ui.State, Commit, [Text]))
    -- ^ (state, commit, name of the cmd this is a checkpoint of)
load repo maybeCommit = tryE "load" $ do
    -- TODO have to handle both compact and expanded tracks
    commit <- defaultHead repo maybeCommit
    commitData <- Git.readCommit repo commit
    dirs <- Git.readDir repo (Git.commitTree commitData)
    names <- parseNames (Git.commitText commitData)
    eitherViews <- loadViews repo
    pure $ do
        state <- undump dirs
        views <- withMsg "views" eitherViews
        pure (state { Ui.stateViews = views }, commit, names)

-- | Try to go get the previous history entry.
loadPreviousHistory :: Git.Repo -> Ui.State -> Commit
    -> IO (Either Text (Maybe LoadHistory))
loadPreviousHistory repo state commit = tryE "load_previous_history" $ do
    commitData <- Git.readCommit repo commit
    case Lists.head (Git.commitParents commitData) of
        Nothing -> pure $ Right Nothing
        Just parent -> loadHistory repo state commit parent

-- | Try to a commits that has this one as a parent.
loadNextHistory :: Git.Repo -> Ui.State -> Commit
    -> IO (Either Text (Maybe LoadHistory))
loadNextHistory repo state commit = tryE "load_next_history" $ do
    -- This won't work if I loaded something off-head.  In that case, I need
    -- the checkpoint I started from so I can start from there instead of HEAD.
    commits <- Git.readLogHead repo
    case findBefore commit commits of
        Nothing -> pure $ Right Nothing
        Just child -> loadHistory repo state commit child
    where
    findBefore val (x1:x2:xs)
        | val == x2 = Just x1
        | otherwise = findBefore val (x2:xs)
    findBefore _ _ = Nothing

-- | Load a history, either in the past or the future.
--
-- Multiple futures:
-- I get the future by tracing from HEAD.  Then if you undo and redo, that
-- branch will be orphaned, and the next gc will probably delete it.  But if
-- you save there, the tag will probably keep it alive.  Then the next
-- history commit will set the HEAD to this branch, and the old HEAD will only
-- be preserved if it had a ref.
loadHistory :: Git.Repo -> Ui.State -> Commit -> Commit
    -> IO (Either Text (Maybe LoadHistory))
loadHistory repo state fromCommit toCommit = do
    names <- parseNames . Git.commitText
        =<< Git.readCommit repo toCommit
    result <- loadFrom repo fromCommit (Just toCommit) state
    case result of
        Left err -> pure $ Left err
        Right (newState, damage) -> pure $ Right $ Just $
            LoadHistory newState toCommit damage names

loadFrom :: Git.Repo -> Commit -> Maybe Commit -> Ui.State
    -> IO (Either Text (Ui.State, Update.UiDamage))
loadFrom repo commitFrom maybeCommitTo state = do
    commitTo <- defaultHead repo maybeCommitTo
    mods <- Git.diffCommits repo commitFrom commitTo
    pure $ undumpDiff state mods

defaultHead :: Git.Repo -> Maybe Commit -> IO Commit
defaultHead _ (Just commit) = pure commit
defaultHead repo Nothing =
    maybe (Git.throw $ "repo with no HEAD commit: " ++ show repo)
        pure =<< Git.readHeadCommit repo

-- | Each commit saves the name of the command, so when you load it, it still
-- has the proper name.
--
-- TODO save and parse in a more robust way
parseNames :: String -> IO [Text]
parseNames text = case lines text of
    [_, names] -> map txt <$> readIO names
    _ -> errorStack $ "can't parse description: " <> showt text

unparseNames :: String -> [Text] -> String
unparseNames msg names = msg ++ "\n" ++ show names ++ "\n"

-- * views

saveViews :: Git.Repo -> Map ViewId Block.View -> IO ()
saveViews repo =
    void . Serialize.serialize Cmd.Serialize.viewsMagic (repo </> "views")

loadViews :: Git.Repo -> IO (Either Text (Map ViewId Block.View))
loadViews repo =
    Serialize.unserialize Cmd.Serialize.viewsMagic (repo </> "views") >>= \case
        Left (Serialize.IOError exc) | IO.Error.isDoesNotExistError exc ->
            pure $ Right mempty
        Left err -> pure $ Left (pretty err)
        Right views -> pure $ Right views

-- * dump / undump

dump :: Ui.State -> [(FilePath, ByteString)]
dump (Ui.State _views blocks tracks rulers config) =
    dumpMap blocks ++ dumpMap tracks ++ dumpMap rulers
    ++ [("config", Serialize.encode config)]

dumpMap :: (Ident id, Serialize.Serialize a) =>
    Map id a -> [(FilePath, ByteString)]
dumpMap m = do
    (ident, val) <- Map.toAscList m
    pure (idToPath ident, Serialize.encode val)

-- | This will tend to create redundant files, e.g. a block will be written
-- twice if two updates occur on it.  But 'Git.modify_dir' will filter out the
-- extras.
dumpDiff :: Bool -> Ui.State -> [Update.UiUpdate]
    -> ([Text], [Git.Modification])
    -- ^ warnings for updates to values that no longer exist
dumpDiff trackDir state =
    -- I use Left "" as a nop, so filter those out.
    first (filter (not . Text.null)) . Either.partitionEithers . map mk
    where
    mk (Update.View {}) = Left ""
    mk u@(Update.Block blockId _)
        | Just block <- Map.lookup blockId (Ui.stateBlocks state) =
            Right $ Git.Add (idToPath blockId) (Serialize.encode block)
        | otherwise = Left $ "block_id: " <> pretty u
    mk u@(Update.Track trackId update)
        | Just track <- Map.lookup trackId (Ui.stateTracks state) =
            case update of
                Update.TrackEvents start end | trackDir ->
                    Right $ dumpEvents state trackId start end
                _ -> Right $
                    Git.Add (idToPath trackId) (Serialize.encode track)
        | otherwise = Left $ "track_id: " <> pretty u
    mk (Update.Ruler rulerId)
        | Just ruler <- Map.lookup rulerId (Ui.stateRulers state) =
            Right $ Git.Add (idToPath rulerId) (Serialize.encode ruler)
        | otherwise = Left $ "ruler_id: " <> pretty rulerId
    mk (Update.State update) = case update of
        Update.Config config ->
            Right $ Git.Add "config" (Serialize.encode config)
        Update.CreateBlock blockId block ->
            Right $ Git.Add (idToPath blockId) (Serialize.encode block)
        Update.DestroyBlock blockId ->
            Right $ Git.Remove (idToPath blockId)
        Update.CreateTrack trackId track ->
            Right $ Git.Add (idToPath trackId) (Serialize.encode track)
        Update.DestroyTrack trackId ->
            Right $ Git.Remove (idToPath trackId)
        Update.CreateRuler rulerId ruler ->
            Right $ Git.Add (idToPath rulerId) (Serialize.encode ruler)
        Update.DestroyRuler rulerId ->
            Right $ Git.Remove (idToPath rulerId)

undump :: Git.Dir -> Either Text Ui.State
undump dir = do
    blocks <- undumpMap Id.BlockId =<< getDir "blocks"
    tracks <- undumpMap Id.TrackId =<< getDir "tracks"
    rulers <- undumpMap Id.RulerId =<< getDir "rulers"
    config <- decode "config" =<< getFile "config"
    pure $ Ui.State mempty blocks tracks rulers config
    where
    getDir name = case Map.lookup name dir of
        Nothing -> pure Map.empty
        Just (Git.File _) -> Left $ "expected dir but got file: " <> showt name
        Just (Git.Dir dir) -> pure dir
    getFile name = case Map.lookup name dir of
        Nothing -> Left $ "file not found: " <> showt name
        Just (Git.Dir _) -> Left $ "expected file but got dir: " <> showt name
        Just (Git.File bytes) -> pure bytes

undumpMap :: (Serialize.Serialize a, Ord id) =>
    (Id.Id -> id) -> Map Git.FileName Git.File -> Either Text (Map id a)
undumpMap mkid dir =
    Map.fromList . concat <$> mapM dirSubs (Map.toAscList dir)
    where
    dirSubs (name, Git.File _) =
        Left $ "expected dir but got file: " <> showt name
    dirSubs (name, Git.Dir subs) = mapM (undumpFile name) (Map.toList subs)
    undumpFile _ (name, Git.Dir _) =
        Left $ "expected file but got dir: " <> showt name
    undumpFile ns (name, Git.File bytes) =
        (,) (pathToId mkid ns name) <$> decode (txt name) bytes

undumpDiff :: Ui.State -> [Git.Modification]
    -> Either Text (Ui.State, Update.UiDamage)
undumpDiff state = foldM apply (state, mempty)
    where
    apply (state, update) (Git.Remove path) = case split path of
        ["blocks", ns, name] -> delete ns name Id.BlockId Ui.blocks
        ["tracks", ns, name] -> delete ns name Id.TrackId Ui.tracks
        ["rulers", ns, name] -> delete ns name Id.RulerId Ui.rulers
        _ -> Left $ "unknown file deleted: " <> showt path
        where
        delete ns name mkid lens = do
            let ident = pathToId mkid ns name
            vals <- deleteKey ident (lens #$ state)
            pure ((lens #= vals) state, update)
    apply (state, update) (Git.Add path bytes) = case split path of
        ["blocks", ns, name] -> (, update) <$> add ns name Id.BlockId Ui.blocks
        ["tracks", ns, name] -> do
            stateTo <- add ns name Id.TrackId Ui.tracks
            let tid = pathToId Id.TrackId ns name
            -- I don't save the UiDamage with the checkpoint, so to avoid
            -- having to rederive the entire track I do a little mini-diff
            -- just on the track.  It shouldn't be too expensive because it's
            -- only on one track at a time.
            let eventUpdates = Diff.trackDiff state stateTo tid
            pure (stateTo, eventUpdates <> update)
        ["rulers", ns, name] -> do
            stateTo <- add ns name Id.RulerId Ui.rulers
            let rid = Set.singleton $ pathToId Id.RulerId ns name
            pure (stateTo, mempty { Update._rulers = rid } <> update)
        ["config"] -> do
            val <- decode (txt path) bytes
            pure ((Ui.config #= val) state, update)
        _ -> Left $ "unknown file modified: " <> showt path
        where
        add ns name mkid lens = do
            let ident = pathToId mkid ns name
            val <- decode (txt path) bytes
            pure $ (lens %= Map.insert ident val) state
    split = FilePath.splitDirectories

class Ident id where idToPath :: id -> FilePath
instance Ident BlockId where idToPath = makeIdPath "blocks"
instance Ident TrackId where idToPath = makeIdPath "tracks"
instance Ident RulerId where idToPath = makeIdPath "rulers"

makeIdPath :: (Id.Ident a) => FilePath -> a -> FilePath
makeIdPath dir id = dir </> untxt nsdir </> untxt name
    where
    (ns, name) = Id.unId (Id.unpackId id)
    nsdir
        | ns == Id.globalNamespace = "*GLOBAL*"
        | otherwise = Id.unNamespace ns

pathToId :: (Id.Id -> id) -> FilePath -> FilePath -> id
pathToId mkid ns name = mkid (Id.id saveNs (txt name))
    where
    saveNs
        | ns == "*GLOBAL*" = Id.globalNamespace
        | otherwise = Id.namespace (txt ns)

-- * util

-- | If a string looks like a commit hash, return the commit, otherwise look
-- for a ref in tags\/.
inferCommit :: Git.Repo -> String -> IO (Maybe Commit)
inferCommit repo refOrCommit = case Git.parseCommit refOrCommit of
    Just commit -> pure $ Just commit
    Nothing -> Git.readRef repo ("tags" </> refOrCommit)

catch :: IO a -> IO (Maybe a)
catch io = do
    result <- Exception.try io
    pure $ case result of
        Left (Git.GitException _) -> Nothing
        Right val -> Just val

try :: Text -> IO a -> IO (Either Text a)
try caller io = do
    result <- Exception.try io
    pure $ case result of
        Left (Git.GitException err) -> Left $ caller <> ": " <> txt err
        Right val -> Right val

tryE :: Text -> IO (Either Text a) -> IO (Either Text a)
tryE caller io = do
    result <- Exception.try io
    pure $ case result of
        Left (Git.GitException err) -> Left $ caller <> ": " <> txt err
        Right (Left err) -> Left $ caller <> ": " <> err
        Right (Right val) -> Right val

deleteKey :: (Show k, Ord k) => k -> Map k a -> Either Text (Map k a)
deleteKey k m
    | Map.member k m = Right $ Map.delete k m
    | otherwise = Left $ "deleted key " <> showt k <> " not present: "
        <> showt (Map.keys m)

decode :: Serialize.Serialize a => Text -> ByteString -> Either Text a
decode msg = withMsg msg . first txt . Serialize.decode

-- | Annotate a failable computation.
withMsg :: Text -> Either Text a -> Either Text a
withMsg msg (Left err) = Left $ msg <> ": " <> err
withMsg _ (Right val) = Right val

-- * config

getUser :: IO (Either Text SaveGitT.User)
getUser = Exception.handle handle $ do
    lines <- Text.lines . txt <$> Process.readProcess "git"
        ["config", "--get-regexp", "user\\.(name|email)"] ""
    let m = Text.strip <$> Map.fromList (map (Text.break (==' ')) lines)
    pure $ case (Map.lookup "user.name" m, Map.lookup "user.email" m) of
        (Just name, Just email)
            | not (Text.null name) && not (Text.null email) ->
                Right $ SaveGitT.User name email
        _ -> Left $ "user.name and user.email not set in git config output: "
            <> showt lines
    where
    handle exc = pure $
        Left $ "error getting git config: " <> showt (exc :: IO.Error.IOError)
