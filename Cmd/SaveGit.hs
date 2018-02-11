-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Cmd.SaveGit (
    SaveHistory(..), LoadHistory(..), Repo, Commit
    -- * save point
    , is_git, git_suffix
    , SavePoint, set_save_tag, write_save_ref, read_save_ref
    , read_last_save, ref_to_save
    -- * save
    , checkpoint, save, should_record
    -- * load
    , load, load_previous_history, load_next_history
    -- * views
    , save_views, load_views
    -- * util
    , infer_commit, try
    -- * User
    , User(..), get_user
#ifdef TESTING
    , parse_names, load_from
#endif
) where
import qualified Control.Exception as Exception
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Numeric
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process

import qualified Util.File as File
import qualified Util.Git as Git
import Util.GitTypes (Commit, Repo)
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import Cmd.SaveGitTypes (SaveHistory(..))
import qualified Cmd.Serialize
import Global
import Types


-- | History loaded from disk.  It only has CmdUpdates so you can feed them to
-- diff.
data LoadHistory = LoadHistory !Ui.State !Commit ![Update.CmdUpdate] ![Text]
    deriving (Show)

is_git :: FilePath -> Bool
is_git = (git_suffix `List.isSuffixOf`)

git_suffix :: FilePath
git_suffix = ".git"

-- * save point

-- | Add a new save point tag to the given commit, unless it already has one.
set_save_tag :: Git.Repo -> Commit -> IO (Either Text SavePoint)
set_save_tag repo commit = try "set_save_tag" $ do
    Git.gc repo -- Might as well clean things up at this point.
    read_last_save repo (Just commit) >>= \case
        Nothing -> do
            save <- find_next_save repo (SavePoint [])
            write_save_ref repo save commit
            return save
        Just (last_save, save_commit) -> do
            save <- find_next_save repo last_save
            if commit == save_commit then return last_save
                else write_save_ref repo save commit >> return save

-- | Stored in reverse order as in the ref name.
newtype SavePoint = SavePoint [Int] deriving (Eq, Show, Pretty)

-- | Create a tag for the given commit.
write_save_ref :: Git.Repo -> SavePoint -> Commit -> IO ()
write_save_ref repo save commit = Git.write_ref repo commit (save_to_ref save)

read_save_ref :: Git.Repo -> SavePoint -> IO (Maybe Commit)
read_save_ref repo save = Git.read_ref repo (save_to_ref save)

read_last_save :: Git.Repo -> Maybe Commit
    -- ^ Find the last save from this commit, or HEAD if not given.
    -> IO (Maybe (SavePoint, Commit))
read_last_save repo maybe_commit = do
    -- This may be called on incomplete repos without a HEAD.
    maybe_commits <- catch $
        maybe (Git.read_log_head repo) (Git.read_log_from repo) maybe_commit
    let commits = fromMaybe [] maybe_commits
    refs <- Git.read_ref_map repo
    let commit_to_save = Map.fromList $ do
            (ref, commit) <- Map.toList refs
            Just save <- return $ ref_to_save ref
            return (commit, save)
    return $ Seq.head [(save, commit) | (Just save, commit)
        <- zip (map (`Map.lookup` commit_to_save) commits) commits]

-- | Find the SavePoint that should be added after the given SavePoint.
find_next_save :: Git.Repo -> SavePoint -> IO SavePoint
find_next_save repo save =
    from_just =<< findM save_free (iterate split (increment save))
    where
    save_free save = Maybe.isNothing <$> Git.read_ref repo (save_to_ref save)
    from_just = maybe -- This should never happen since iterate tries forever.
        (Git.throw $ "couldn't find a free save name after " ++ show save)
        return
    split (SavePoint xs) = SavePoint (0 : xs)
    increment (SavePoint []) = SavePoint [0]
    increment (SavePoint (x:xs)) = SavePoint (x + 1 : xs)

ref_to_save :: Git.Ref -> Maybe SavePoint
ref_to_save ref
    | not (all (all Char.isDigit) versions) = Nothing
    | otherwise = Just $ SavePoint (reverse (map read versions))
    where
    (save, _) = Seq.drop_prefix "tags/" ref
    versions = Seq.split "." save

save_to_ref :: SavePoint -> Git.Ref
save_to_ref (SavePoint versions) =
    "tags" </> Seq.join "." (map show (reverse versions))


-- * save

-- | Checkpoint the given SaveHistory.  If it has no previous commit, create
-- a new repo.
checkpoint :: User -> Git.Repo -> SaveHistory -> IO (Either Text Commit)
checkpoint user repo (SaveHistory state Nothing _ names) =
    try "save" $ save user repo state names
checkpoint user repo (SaveHistory state (Just commit) updates names) =
        try "checkpoint" $ do
    let (not_found, mods) = dump_diff False state (filter should_record updates)
    unless (null not_found) $
        Log.warn $ "ignored updates for nonexistent "
            <> Text.intercalate ", " not_found
            <> "; this probably means 'Ui.Diff.cancel_updates didn't do its job"
    if null mods then return commit else do
        unlessM (File.writable repo) $
            Git.throw $ "git repo is not writable: " <> show repo
        last_tree <- Git.commit_tree <$> Git.read_commit repo commit
        tree <- Git.modify_tree repo last_tree mods
        commit_tree user repo tree (Just commit)
            (unparse_names "checkpoint" names)

-- | Create a new repo, or throw if it already exists.
save :: User -> Git.Repo -> Ui.State -> [Text] -> IO Commit
save user repo state cmd_names = do
    whenM (Git.init repo) $
        Git.throw "refusing to overwrite a repo that already exists"
    dir <- either (Git.throw . ("make_dir: "<>)) return $
        Git.make_dir (dump state)
    tree <- Git.write_dir repo dir
    commit_tree user repo tree Nothing (unparse_names "save" cmd_names)

-- | True if this update is interesting enough to record a checkpoint for.
should_record :: Update.UiUpdate -> Bool
should_record update = case update of
    -- BlockConfig changes are only box colors, which I never need to save.
    Update.Block _ (Update.BlockConfig {}) -> False
    _ -> not $ Update.is_view_update update

commit_tree :: User -> Git.Repo -> Git.Tree -> Maybe Commit -> String
    -> IO Commit
commit_tree user repo tree maybe_parent desc = do
    commit <- Git.write_commit repo (name user) (email user)
        (maybe [] (:[]) maybe_parent) tree desc
    Git.update_head repo commit
    return commit

-- ** events update

-- | TODO disabled event because loading isn't implemented.  Should probably
-- just delete it all.
dump_events :: Ui.State -> TrackId -> ScoreTime -> ScoreTime
    -> Git.Modification
dump_events state track_id start end =
    Git.Add (events_path track_id start end) $
        Serialize.encode $ EventsUpdate track_id start end events
    where
    events = maybe Events.empty
        (Events.in_range (Events.Range start end) . Track.track_events)
        (Map.lookup track_id (Ui.state_tracks state))

-- | Put the range into the filename.  You still have to load all the event
-- files in the directory, but at least exactly matching ranges will overwrite
-- each other.
events_path :: TrackId -> ScoreTime -> ScoreTime -> String
events_path track_id start end = id_to_path track_id ++ "_"
    </> score_to_hex start ++ "-" ++ score_to_hex end

data EventsUpdate = EventsUpdate TrackId ScoreTime ScoreTime Events.Events
    deriving (Show)

instance Serialize.Serialize EventsUpdate where
    put (EventsUpdate a b c d) = Serialize.put a >> Serialize.put b
        >> Serialize.put c >> Serialize.put d
    get = EventsUpdate <$> Serialize.get <*> Serialize.get <*> Serialize.get
        <*> Serialize.get

score_to_hex :: ScoreTime -> String
score_to_hex = pad . flip Numeric.showHex "" . Serialize.encode_double
    . ScoreTime.to_double
    where pad s = replicate (16 - length s) '0' ++ s


-- * load

load :: Git.Repo -> Maybe Commit
    -> IO (Either Text (Ui.State, Commit, [Text]))
    -- ^ (state, commit, name of the cmd this is a checkpoint of)
load repo maybe_commit = try_e "load" $ do
    -- TODO have to handle both compact and expanded tracks
    commit <- default_head repo maybe_commit
    commit_data <- Git.read_commit repo commit
    dirs <- Git.read_dir repo (Git.commit_tree commit_data)
    names <- parse_names (Git.commit_text commit_data)
    either_views <- load_views repo
    return $ do
        state <- undump dirs
        views <- with_msg "views" either_views
        return (state { Ui.state_views = views }, commit, names)

-- | Try to go get the previous history entry.
load_previous_history :: Git.Repo -> Ui.State -> Commit
    -> IO (Either Text (Maybe LoadHistory))
load_previous_history repo state commit = try_e "load_previous_history" $ do
    commit_data <- Git.read_commit repo commit
    case Seq.head (Git.commit_parents commit_data) of
        Nothing -> return $ Right Nothing
        Just parent -> load_history repo state commit parent

-- | Try to a commits that has this one as a parent.
load_next_history :: Git.Repo -> Ui.State -> Commit
    -> IO (Either Text (Maybe LoadHistory))
load_next_history repo state commit = try_e "load_next_history" $ do
    -- This won't work if I loaded something off-head.  In that case, I need
    -- the checkpoint I started from so I can start from there instead of HEAD.
    commits <- Git.read_log_head repo
    case find_before commit commits of
        Nothing -> return $ Right Nothing
        Just child -> load_history repo state commit child
    where
    find_before val (x1:x2:xs)
        | val == x2 = Just x1
        | otherwise = find_before val (x2:xs)
    find_before _ _ = Nothing

-- | Load a history, either in the past or the future.
--
-- Multiple futures:
-- I get the future by tracing from HEAD.  Then if you undo and redo, that
-- branch will be orphaned, and the next gc will probably delete it.  But if
-- you save there, the tag will probably keep it alive.  Then the next
-- history commit will set the HEAD to this branch, and the old HEAD will only
-- be preserved if it had a ref.
load_history :: Git.Repo -> Ui.State -> Commit -> Commit
    -> IO (Either Text (Maybe LoadHistory))
load_history repo state from_commit to_commit = do
    names <- parse_names . Git.commit_text
        =<< Git.read_commit repo to_commit
    result <- load_from repo from_commit (Just to_commit) state
    case result of
        Left err -> return $ Left err
        Right (new_state, cmd_updates) -> return $ Right $ Just
            (LoadHistory new_state to_commit cmd_updates names)

load_from :: Git.Repo -> Commit -> Maybe Commit -> Ui.State
    -> IO (Either Text (Ui.State, [Update.CmdUpdate]))
load_from repo commit_from maybe_commit_to state = do
    commit_to <- default_head repo maybe_commit_to
    mods <- Git.diff_commits repo commit_from commit_to
    return $ undump_diff state mods

default_head :: Git.Repo -> Maybe Commit -> IO Commit
default_head _ (Just commit) = return commit
default_head repo Nothing =
    maybe (Git.throw $ "repo with no HEAD commit: " ++ show repo)
        return =<< Git.read_head_commit repo

-- | Each commit saves the name of the command, so when you load it, it still
-- has the proper name.
--
-- TODO save and parse in a more robust way
parse_names :: String -> IO [Text]
parse_names text = case lines text of
    [_, names] -> map txt <$> readIO names
    _ -> errorStack $ "can't parse description: " <> showt text

unparse_names :: String -> [Text] -> String
unparse_names msg names = msg ++ "\n" ++ show names ++ "\n"

-- * views

save_views :: Git.Repo -> Map ViewId Block.View -> IO ()
save_views repo =
    void . Serialize.serialize Cmd.Serialize.views_magic (repo </> "views")

load_views :: Git.Repo -> IO (Either Text (Map ViewId Block.View))
load_views repo =
    Serialize.unserialize Cmd.Serialize.views_magic (repo </> "views") >>= \case
        Left (Serialize.IOError exc) | IO.Error.isDoesNotExistError exc ->
            return $ Right mempty
        Left err -> return $ Left (pretty err)
        Right views -> return $ Right views

-- * dump / undump

dump :: Ui.State -> [(FilePath, ByteString)]
dump (Ui.State _views blocks tracks rulers config) =
    dump_map blocks ++ dump_map tracks ++ dump_map rulers
    ++ [("config", Serialize.encode config)]

dump_map :: (Ident id, Serialize.Serialize a) =>
    Map id a -> [(FilePath, ByteString)]
dump_map m = do
    (ident, val) <- Map.toAscList m
    return (id_to_path ident, Serialize.encode val)

-- | This will tend to create redundant files, e.g. a block will be written
-- twice if two updates occur on it.  But 'Git.modify_dir' will filter out the
-- extras.
dump_diff :: Bool -> Ui.State -> [Update.UiUpdate]
    -> ([Text], [Git.Modification])
    -- ^ warnings for updates to values that no longer exist
dump_diff track_dir state =
    -- I use Left "" as a nop, so filter those out.
    first (filter (not . Text.null)) . Either.partitionEithers . map mk
    where
    mk (Update.View {}) = Left ""
    mk u@(Update.Block block_id _)
        | Just block <- Map.lookup block_id (Ui.state_blocks state) =
            Right $ Git.Add (id_to_path block_id) (Serialize.encode block)
        | otherwise = Left $ "block_id: " <> pretty u
    mk u@(Update.Track track_id update)
        | Just track <- Map.lookup track_id (Ui.state_tracks state) =
            case update of
                Update.TrackEvents start end | track_dir ->
                    Right $ dump_events state track_id start end
                _ -> Right $
                    Git.Add (id_to_path track_id) (Serialize.encode track)
        | otherwise = Left $ "track_id: " <> pretty u
    mk (Update.Ruler ruler_id)
        | Just ruler <- Map.lookup ruler_id (Ui.state_rulers state) =
            Right $ Git.Add (id_to_path ruler_id) (Serialize.encode ruler)
        | otherwise = Left $ "ruler_id: " <> pretty ruler_id
    mk (Update.State update) = case update of
        Update.Config config ->
            Right $ Git.Add "config" (Serialize.encode config)
        Update.CreateBlock block_id block ->
            Right $ Git.Add (id_to_path block_id) (Serialize.encode block)
        Update.DestroyBlock block_id ->
            Right $ Git.Remove (id_to_path block_id)
        Update.CreateTrack track_id track ->
            Right $ Git.Add (id_to_path track_id) (Serialize.encode track)
        Update.DestroyTrack track_id ->
            Right $ Git.Remove (id_to_path track_id)
        Update.CreateRuler ruler_id ruler ->
            Right $ Git.Add (id_to_path ruler_id) (Serialize.encode ruler)
        Update.DestroyRuler ruler_id ->
            Right $ Git.Remove (id_to_path ruler_id)

undump :: Git.Dir -> Either Text Ui.State
undump dir = do
    blocks <- undump_map Id.BlockId =<< get_dir "blocks"
    tracks <- undump_map Id.TrackId =<< get_dir "tracks"
    rulers <- undump_map Id.RulerId =<< get_dir "rulers"
    config <- decode "config" =<< get_file "config"
    return $ Ui.State mempty blocks tracks rulers config
    where
    get_dir name = case Map.lookup name dir of
        Nothing -> return Map.empty
        Just (Git.File _) -> Left $ "expected dir but got file: " <> showt name
        Just (Git.Dir dir) -> return dir
    get_file name = case Map.lookup name dir of
        Nothing -> Left $ "file not found: " <> showt name
        Just (Git.Dir _) -> Left $ "expected file but got dir: " <> showt name
        Just (Git.File bytes) -> return bytes

undump_map :: (Serialize.Serialize a, Ord id) =>
    (Id.Id -> id) -> Map Git.FileName Git.File -> Either Text (Map id a)
undump_map mkid dir =
    Map.fromList . concat <$> mapM dir_subs (Map.toAscList dir)
    where
    dir_subs (name, Git.File _) =
        Left $ "expected dir but got file: " <> showt name
    dir_subs (name, Git.Dir subs) = mapM (undump_file name) (Map.toList subs)
    undump_file _ (name, Git.Dir _) =
        Left $ "expected file but got dir: " <> showt name
    undump_file ns (name, Git.File bytes) =
        (,) (path_to_id mkid ns name) <$> decode (txt name) bytes

undump_diff :: Ui.State -> [Git.Modification]
    -> Either Text (Ui.State, [Update.CmdUpdate])
undump_diff state = foldM apply (state, [])
    where
    apply (state, updates) (Git.Remove path) = case split path of
        ["blocks", ns, name] -> delete ns name Id.BlockId Ui.blocks
        ["tracks", ns, name] -> delete ns name Id.TrackId Ui.tracks
        ["rulers", ns, name] -> delete ns name Id.RulerId Ui.rulers
        _ -> Left $ "unknown file deleted: " <> showt path
        where
        delete ns name mkid lens = do
            let ident = path_to_id mkid ns name
            vals <- delete_key ident (lens #$ state)
            return ((lens #= vals) state, updates)
    apply (state, updates) (Git.Add path bytes) = case split path of
        ["blocks", ns, name] -> add ns name Id.BlockId Ui.blocks
        ["tracks", ns, name] -> do
            (state_to, updates) <- add ns name Id.TrackId Ui.tracks
            let tid = path_to_id Id.TrackId ns name
            -- I don't save the CmdUpdates with the checkpoint, so to avoid
            -- having to rederive the entire track I do a little mini-diff
            -- just on the track.  It shouldn't be too expensive because it's
            -- only on one track at a time.
            let event_updates = Diff.track_diff state state_to tid
            return (state_to, event_updates ++ updates)
        ["rulers", ns, name] -> do
            (state_to, updates) <- add ns name Id.RulerId Ui.rulers
            let rid = path_to_id Id.RulerId ns name
            return (state_to, Update.CmdRuler rid : updates)
        ["config"] -> do
            val <- decode (txt path) bytes
            return ((Ui.config #= val) state, updates)
        _ -> Left $ "unknown file modified: " <> showt path
        where
        add ns name mkid lens = do
            let ident = path_to_id mkid ns name
            val <- decode (txt path) bytes
            return ((lens %= Map.insert ident val) state, updates)
    split = FilePath.splitDirectories

class Ident id where id_to_path :: id -> FilePath
instance Ident BlockId where id_to_path = make_id_path "blocks"
instance Ident TrackId where id_to_path = make_id_path "tracks"
instance Ident RulerId where id_to_path = make_id_path "rulers"

make_id_path :: (Id.Ident a) => FilePath -> a -> FilePath
make_id_path dir id = dir </> untxt nsdir </> untxt name
    where
    (ns, name) = Id.un_id (Id.unpack_id id)
    nsdir
        | ns == Id.global_namespace = "*GLOBAL*"
        | otherwise = Id.un_namespace ns

path_to_id :: (Id.Id -> id) -> FilePath -> FilePath -> id
path_to_id mkid ns name = mkid (Id.id save_ns (txt name))
    where
    save_ns
        | ns == "*GLOBAL*" = Id.global_namespace
        | otherwise = Id.namespace (txt ns)

-- * util

-- | If a string looks like a commit hash, return the commit, otherwise look
-- for a ref in tags\/.
infer_commit :: Git.Repo -> String -> IO (Maybe Commit)
infer_commit repo ref_or_commit = case Git.parse_commit ref_or_commit of
    Just commit -> return $ Just commit
    Nothing -> Git.read_ref repo ("tags" </> ref_or_commit)

catch :: IO a -> IO (Maybe a)
catch io = do
    result <- Exception.try io
    return $ case result of
        Left (Git.GitException _) -> Nothing
        Right val -> Just val

try :: Text -> IO a -> IO (Either Text a)
try caller io = do
    result <- Exception.try io
    return $ case result of
        Left (Git.GitException err) -> Left $ caller <> ": " <> txt err
        Right val -> Right val

try_e :: Text -> IO (Either Text a) -> IO (Either Text a)
try_e caller io = do
    result <- Exception.try io
    return $ case result of
        Left (Git.GitException err) -> Left $ caller <> ": " <> txt err
        Right (Left err) -> Left $ caller <> ": " <> err
        Right (Right val) -> Right val

delete_key :: (Show k, Ord k) => k -> Map k a -> Either Text (Map k a)
delete_key k m
    | Map.member k m = Right $ Map.delete k m
    | otherwise = Left $ "deleted key " <> showt k <> " not present: "
        <> showt (Map.keys m)

decode :: Serialize.Serialize a => Text -> ByteString -> Either Text a
decode msg = with_msg msg . first txt . Serialize.decode

-- | Annotate a failable computation.
with_msg :: Text -> Either Text a -> Either Text a
with_msg msg (Left err) = Left $ msg <> ": " <> err
with_msg _ (Right val) = Right val

-- * config

-- | Git wants these fields for commits.  It probably doesn't matter much
-- what they are, but they might as well be accurate.
data User = User { name :: !Text, email :: !Text } deriving (Show)

get_user :: IO (Either Text User)
get_user = Exception.handle handle $ do
    lines <- Text.lines . txt <$> Process.readProcess "git"
        ["config", "--get-regexp", "user.(name|email)"] ""
    let m = Text.strip <$> Map.fromList (map (Text.break (==' ')) lines)
    return $ case (Map.lookup "user.name" m, Map.lookup "user.email" m) of
        (Just name, Just email)
            | not (Text.null name) && not (Text.null email) ->
                Right $ User name email
        _ -> Left $ "user.name and user.email not set in git config output: "
            <> showt lines
    where
    handle exc = return $
        Left $ "error getting git config: " <> showt (exc :: IO.Error.IOError)
