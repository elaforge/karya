{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cmd.SaveGit (module Cmd.SaveGit, Git.Commit, Git.Repo) where
import qualified Control.Exception as Exception
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Numeric
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.Git as Git
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Ui.Diff as Diff
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import Cmd.Serialize ()
import qualified App.Config as Config
import Types


-- | History loaded from disk.  It only has CmdUpdates so you can feed them to
-- diff.
data LoadHistory =
    LoadHistory !State.State !Git.Commit ![Update.CmdUpdate] ![String]
    deriving (Show)

-- | History to be saved to disk.  The updates are post-diff to know which bits
-- of state to write, and the commit is what commit the updates are relative
-- to, if any.  If they're Nothing, then save everything.
--
-- It's very important to bundle the commit and updates together, because
-- without the commit to know what they are relative to, the updates don't
-- mean anything, and if they're applied on top of the wrong commit the result
-- will be a corrupted state.
data SaveHistory =
    SaveHistory !State.State !(Maybe Git.Commit) [Update.UiUpdate] ![String]
    deriving (Show)

instance Pretty.Pretty SaveHistory where
    format (SaveHistory _state commit updates cmds) =
        Pretty.record_title "SaveHistory"
            [ ("commit", Pretty.format commit)
            , ("updates", Pretty.format updates)
            , ("cmds", Pretty.format cmds)
            ]

is_git :: FilePath -> Bool
is_git = (".git" `List.isSuffixOf`)

-- * save

-- | Like 'checkpoint', but save everything all over again, and add a tag too.
-- It will create a new repo if the given path doesn't exist.
--
-- TODO it shouldn't be necessary to save everything, and it's slow
save :: Git.Repo -> State.State -> Maybe Git.Commit
    -> IO (Either String (Git.Commit, SavePoint))
save repo state prev_commit = try "save" $
    do_save repo (SaveHistory state prev_commit [] ["save"])

do_save :: Git.Repo -> SaveHistory -> IO (Git.Commit, SavePoint)
do_save repo (SaveHistory state prev_commit _updates names) = do
    exists <- Git.init repo
    prev_commit <- case prev_commit of
        Just c -> return $ if exists then Just c else Nothing
        Nothing -> do
            when exists $
                Git.throw "refusing to overwrite a repo that already exists"
            return Nothing
    dir <- either (Git.throw . ("make_dir: "++)) return $
        Git.make_dir (dump state)
    tree <- Git.write_dir repo dir
    last_save <- read_last_save repo prev_commit
    save <- find_next_save repo (fromMaybe (SavePoint []) last_save)
    commit <- commit_tree repo tree prev_commit $
        unparse_names ("save " ++ save_to_ref save) names
    write_save_ref repo save commit
    Git.gc repo
    return (commit, save)

-- | Stored in reverse order as in the ref name.
newtype SavePoint = SavePoint [Int] deriving (Eq, Show, Pretty.Pretty)

-- | Create a tag for the given commit.
write_save_ref :: Git.Repo -> SavePoint -> Git.Commit -> IO ()
write_save_ref repo save commit = Git.write_ref repo commit (save_to_ref save)

read_save_ref :: Git.Repo -> SavePoint -> IO (Maybe Git.Commit)
read_save_ref repo save = Git.read_ref repo (save_to_ref save)

read_last_save :: Git.Repo -> Maybe Git.Commit
    -- ^ Find the last save from this commit, or HEAD if not given.
    -> IO (Maybe SavePoint)
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
    return $ msum $ map (`Map.lookup` commit_to_save) commits

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


-- * checkpoint

checkpoint :: Git.Repo -> SaveHistory -> IO (Either String Git.Commit)
checkpoint repo hist@(SaveHistory state prev_commit updates names) =
        try_e "checkpoint" $ do
    -- If this is a new repo then do a save instead.
    case prev_commit of
        Nothing -> Right . fst <$> do_save repo hist
        Just prev_commit -> do
            let (warns, mods) = dump_diff False state
                    (filter checkpoint_update updates)
            unless (null warns) $
                Log.warn $ "ignored updates for nonexistent "
                    ++ Seq.join ", " warns
                    ++ "; this probably means 'Ui.Diff.cancel_updates didn't"
                    ++ " do its job"
            last_tree <- Git.commit_tree <$> Git.read_commit repo prev_commit
            tree <- Git.modify_tree repo last_tree mods
            commit <- commit_tree repo tree (Just prev_commit) $
                unparse_names "checkpoint" names
            return $ Right commit
    where
    checkpoint_update update = case update of
        -- BlockConfig changes are only box colors, which I never need to save.
        Update.Block _ (Update.BlockConfig {}) -> False
        Update.View _ (Update.Status {}) -> False
        Update.View _ Update.BringToFront -> False
        _ -> True

commit_tree :: Git.Repo -> Git.Tree -> Maybe Git.Commit -> String
    -> IO Git.Commit
commit_tree repo tree maybe_parent desc = do
    commit <- Git.write_commit repo Config.name Config.email
        (maybe [] (:[]) maybe_parent) tree desc
    Git.update_head repo commit
    return commit

-- ** events update

-- | TODO disabled event because loading isn't implemented.  Should probably
-- just delete it all.
dump_events :: State.State -> TrackId -> ScoreTime -> ScoreTime
    -> Git.Modification
dump_events state track_id start end =
    Git.Add (events_path track_id start end) $
        Serialize.encode $ EventsUpdate track_id start end events
    where
    events = maybe Events.empty
        (Events.in_range start end . Track.track_events)
        (Map.lookup track_id (State.state_tracks state))

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

load :: Git.Repo -> Maybe Git.Commit
    -> IO (Either String (State.State, Git.Commit, [String]))
    -- ^ (state, commit, cmd_names)
load repo maybe_commit = try_e "load" $ do
    -- TODO have to handle both compact and expanded tracks
    commit <- default_head repo maybe_commit
    commit_data <- Git.read_commit repo commit
    dirs <- Git.read_dir repo (Git.commit_tree commit_data)
    names <- parse_names (Git.commit_text commit_data)
    return $ do
        state <- undump dirs
        return (state, commit, names)

-- | Try to go get the previous history entry.
load_previous_history :: Git.Repo -> State.State -> Git.Commit
    -> IO (Either String (Maybe LoadHistory))
load_previous_history repo state commit = try_e "load_previous_history" $ do
    commit_data <- Git.read_commit repo commit
    case Seq.head (Git.commit_parents commit_data) of
        Nothing -> return $ Right Nothing
        Just parent -> load_history repo state commit parent

-- | Try to a commits that has this one as a parent.
load_next_history :: Git.Repo -> State.State -> Git.Commit
    -> IO (Either String (Maybe LoadHistory))
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
load_history :: Git.Repo -> State.State -> Git.Commit -> Git.Commit
    -> IO (Either String (Maybe LoadHistory))
load_history repo state from_commit to_commit = do
    names <- parse_names . Git.commit_text
        =<< Git.read_commit repo to_commit
    result <- load_from repo from_commit (Just to_commit) state
    case result of
        Left err -> return $ Left err
        Right (new_state, cmd_updates) -> return $ Right $ Just
            (LoadHistory new_state to_commit cmd_updates names)

load_from :: Git.Repo -> Git.Commit -> Maybe Git.Commit -> State.State
    -> IO (Either String (State.State, [Update.CmdUpdate]))
load_from repo commit_from maybe_commit_to state = do
    commit_to <- default_head repo maybe_commit_to
    mods <- Git.diff_commits repo commit_from commit_to
    return $ undump_diff state mods

default_head :: Git.Repo -> Maybe Git.Commit -> IO Git.Commit
default_head _ (Just commit) = return commit
default_head repo Nothing =
    maybe (Git.throw $ "repo with no HEAD commit: " ++ show repo)
        return =<< Git.read_head_commit repo

parse_names :: String -> IO [String]
parse_names text = case lines text of
    [_, names] -> readIO names
    _ -> error $ "can't parse description: " ++ show text

unparse_names :: String -> [String] -> String
unparse_names msg names = msg ++ "\n" ++ show names ++ "\n"

-- * dump / undump

dump :: State.State -> [(FilePath, ByteString)]
dump (State.State views blocks tracks rulers config) =
    dump_map views ++ dump_map blocks ++ dump_map tracks ++ dump_map rulers
    ++ [("config", Serialize.encode config)]

dump_map :: (Ident id, Serialize.Serialize a) =>
    Map.Map id a -> [(FilePath, ByteString)]
dump_map m = do
    (ident, val) <- Map.toAscList m
    return (id_to_path ident, Serialize.encode val)

-- | This will tend to create redundant files, e.g. a block will be written
-- twice if two updates occur on it.  But 'Git.modify_dir' will filter out the
-- extras.
dump_diff :: Bool -> State.State -> [Update.UiUpdate]
    -> ([String], [Git.Modification])
    -- ^ warnings for updates to values that no longer exist
dump_diff track_dir state =
    -- I use Left "" as a nop, so filter those out.
    first (filter (not . null)) . Seq.partition_either . map mk
    where
    mk u@(Update.View view_id update) = case update of
        Update.DestroyView -> Right $ Git.Remove (id_to_path view_id)
        Update.BringToFront -> Left ""
        _ | Just view <- Map.lookup view_id (State.state_views state) ->
            Right $ Git.Add (id_to_path view_id) (Serialize.encode view)
        _ -> Left $ "view_id: " ++ Pretty.pretty u
    mk u@(Update.Block block_id _)
        | Just block <- Map.lookup block_id (State.state_blocks state) =
            Right $ Git.Add (id_to_path block_id) (Serialize.encode block)
        | otherwise = Left $ "block_id: " ++ Pretty.pretty u
    mk u@(Update.Track track_id update)
        | Just track <- Map.lookup track_id (State.state_tracks state) =
            case update of
                Update.TrackEvents start end | track_dir ->
                    Right $ dump_events state track_id start end
                _ -> Right $
                    Git.Add (id_to_path track_id) (Serialize.encode track)
        | otherwise = Left $ "track_id: " ++ Pretty.pretty u
    mk (Update.Ruler ruler_id)
        | Just ruler <- Map.lookup ruler_id (State.state_rulers state) =
            Right $ Git.Add (id_to_path ruler_id) (Serialize.encode ruler)
        | otherwise = Left $ "ruler_id: " ++ Pretty.pretty ruler_id
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

undump :: Git.Dir -> Either String State.State
undump dir = do
    views <- undump_map Types.ViewId =<< get_dir "views"
    blocks <- undump_map Types.BlockId =<< get_dir "blocks"
    tracks <- undump_map Types.TrackId =<< get_dir "tracks"
    rulers <- undump_map Types.RulerId =<< get_dir "rulers"
    config <- decode "config" =<< get_file "config"
    return $ State.State views blocks tracks rulers config
    where
    get_dir name = case Map.lookup name dir of
        Nothing -> return Map.empty
        Just (Git.File _) -> Left $ "expected dir but got file: " ++ show name
        Just (Git.Dir dir) -> return dir
    get_file name = case Map.lookup name dir of
        Nothing -> Left $ "file not found: " ++ show name
        Just (Git.Dir _) -> Left $ "expected file but got dir: " ++ show name
        Just (Git.File bytes) -> return bytes

undump_map :: (Serialize.Serialize a, Ord id) =>
    (Id.Id -> id) -> Map.Map Git.FileName Git.File
    -> Either String (Map.Map id a)
undump_map mkid dir =
    Map.fromList . concat <$> mapM dir_subs (Map.toAscList dir)
    where
    dir_subs (name, Git.File _) =
        Left $ "expected dir but got file: " ++ show name
    dir_subs (name, Git.Dir subs) = mapM (undump_file name) (Map.toList subs)
    undump_file _ (name, Git.Dir _) =
        Left $ "expected file but got dir: " ++ show name
    undump_file ns (name, Git.File bytes) =
        (,) <$> path_to_id mkid ns name <*> decode name bytes

undump_diff :: State.State -> [Git.Modification]
    -> Either String (State.State, [Update.CmdUpdate])
undump_diff state = foldM apply (state, [])
    where
    apply (state, updates) (Git.Remove path) = case split path of
        ["views", ns, name] -> delete ns name Types.ViewId State.views
        ["blocks", ns, name] -> delete ns name Types.BlockId State.blocks
        ["tracks", ns, name] -> delete ns name Types.TrackId State.tracks
        ["rulers", ns, name] -> delete ns name Types.RulerId State.rulers
        _ -> Left $ "unknown file deleted: " ++ show path
        where
        delete ns name mkid lens = do
            ident <- path_to_id mkid ns name
            vals <- delete_key ident (lens #$ state)
            return ((lens #= vals) state, updates)
    apply (state, updates) (Git.Add path bytes) = case split path of
        ["views", ns, name] -> add ns name Types.ViewId State.views
        ["blocks", ns, name] -> add ns name Types.BlockId State.blocks
        ["tracks", ns, name] -> do
            (state_to, updates) <- add ns name Types.TrackId State.tracks
            tid <- path_to_id Types.TrackId ns name
            -- I don't save the CmdUpdates with the checkpoint, so to avoid
            -- having to rederive the entire track I do a little mini-diff
            -- just on the track.  It shouldn't be too expensive because it's
            -- only on one track at a time.
            let event_updates = Diff.track_diff state state_to tid
            return (state_to, event_updates ++ updates)
        ["rulers", ns, name] -> do
            (state_to, updates) <- add ns name Types.RulerId State.rulers
            rid <- path_to_id Types.RulerId ns name
            return (state_to, Update.CmdRuler rid : updates)
        ["config"] -> do
            val <- decode path bytes
            return ((State.config #= val) state, updates)
        _ -> Left $ "unknown file modified: " ++ show path
        where
        add ns name mkid lens = do
            ident <- path_to_id mkid ns name
            val <- decode path bytes
            return ((lens %= Map.insert ident val) state, updates)
    split = FilePath.splitDirectories

class Ident id where id_to_path :: id -> FilePath
instance Ident ViewId where id_to_path = make_id_path "views"
instance Ident BlockId where id_to_path = make_id_path "blocks"
instance Ident TrackId where id_to_path = make_id_path "tracks"
instance Ident RulerId where id_to_path = make_id_path "rulers"

make_id_path :: (Id.Ident a) => FilePath -> a -> FilePath
make_id_path dir id = dir </> nsdir </> name
    where
    (ns, name) = Id.un_id (Id.unpack_id id)
    nsdir = if ns == Id.global_namespace then "*GLOBAL*"
        else Id.un_namespace ns

path_to_id :: (Id.Id -> id) -> FilePath -> FilePath -> Either String id
path_to_id mkid ns name = do
    ns <- if ns == "*GLOBAL*" then return Id.global_namespace
        else maybe (Left $ "invalid namespace: " ++ show ns) Right
            (Id.namespace ns)
    mkid <$> maybe
        (Left $ "invalid ident name: " ++ show name) Right (Id.id ns name)

-- * util

-- | If a string looks like a commit hash, return the commit, otherwise look
-- for a ref in tags\/.
infer_commit :: Git.Repo -> String -> IO (Maybe Git.Commit)
infer_commit repo ref_or_commit = case Git.parse_commit ref_or_commit of
    Just commit -> return $ Just commit
    Nothing -> Git.read_ref repo ("tags" </> ref_or_commit)

catch :: IO a -> IO (Maybe a)
catch io = do
    result <- Exception.try io
    return $ case result of
        Left (Git.GitException _) -> Nothing
        Right val -> Just val

try :: String -> IO a -> IO (Either String a)
try caller io = do
    result <- Exception.try io
    return $ case result of
        Left (Git.GitException err) -> Left $ caller ++ ": " ++ err
        Right val -> Right val

try_e :: String -> IO (Either String a) -> IO (Either String a)
try_e caller io = do
    result <- Exception.try io
    return $ case result of
        Left (Git.GitException err) -> Left $ caller ++ ": " ++ err
        Right (Left err) -> Left $ caller ++ ": " ++ err
        Right (Right val) -> Right val

delete_key :: (Show k, Ord k) => k -> Map.Map k a -> Either String (Map.Map k a)
delete_key k m
    | Map.member k m = Right $ Map.delete k m
    | otherwise = Left $ "deleted key " ++ show k ++ " not present: "
        ++ show (Map.keys m)

decode :: (Serialize.Serialize a) => String -> ByteString -> Either String a
decode msg = with_msg msg . Serialize.decode

with_msg :: String -> Either String a -> Either String a
with_msg msg (Left err) = Left $ msg ++ ": " ++ err
with_msg _ (Right val) = Right val
