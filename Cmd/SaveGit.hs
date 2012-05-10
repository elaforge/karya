{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cmd.SaveGit where
import qualified Control.Exception as Exception
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Numeric
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.Git as Git
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


-- | A loaded history entry, along with the the updates to take it from the
-- previous entry.
data History update = History !State.State ![update] ![String]
    deriving (Show)

-- | History loaded from disk only has CmdUpdates to feed to diff.
type LoadHistory = History Update.CmdUpdate
-- | When saving a History, though, I need post-diff updates.
type SaveHistory = History Update.UiUpdate

save_repo :: State.State -> Git.Repo
save_repo = save_file True

save_file :: Bool -> State.State -> Git.Repo
save_file for_git state =
    FilePath.combine dir $
        map sanitize (Id.un_namespace ns) ++ if for_git then ".git" else ""
    where
    dir = State.config#State.project_dir #$ state
    ns = State.config#State.namespace #$ state
    -- This shouldn't be necessary because of the Namespace naming
    -- restrictions, but it doesn't hurt to be careful anyway.
    sanitize c = if FilePath.isPathSeparator c then '_' else c

-- * save

-- | Like 'checkpoint', but add a tag too, and write compact tracks.
save :: Git.Repo -> State.State -> IO (Either String (Git.Commit, SavePoint))
save repo state = try "save" $ do_save repo (History state [] ["save"])

do_save :: Git.Repo -> SaveHistory -> IO (Git.Commit, SavePoint)
do_save repo (History state _updates names) = do
    Git.init repo
    tree <- Git.write_dir repo (dump state)
    last_save <- read_last_save repo
    save <- find_next_save repo (Maybe.fromMaybe (SavePoint []) last_save)
    commit <- commit_tree repo tree $
        unparse_names ("save " ++ save_to_ref save) names
    write_save_ref repo save commit
    Git.gc repo
    return (commit, save)

-- HEAD points to the last commit, as usual.  On a full save the previous save
-- number is taken from last-save, a new save tag is created, and last-save is
-- updated to point to it:
--
-- x        x       x       x       x       x
-- tags/1           tags/2                  refs/master
--                  last-save               HEAD

-- | Stored in reverse order as in the ref name.
newtype SavePoint = SavePoint [Int] deriving (Eq, Show, Pretty.Pretty)

-- | Create a tag for the given commit, and point last-save at it.
write_save_ref :: Git.Repo -> SavePoint -> Git.Commit -> IO ()
write_save_ref repo save commit = do
    let ref = save_to_ref save
    Git.write_ref repo commit ref
    Git.write_symbolic_ref repo "last-save" ref

read_save_ref :: Git.Repo -> SavePoint -> IO (Maybe Git.Commit)
read_save_ref repo save = Git.read_ref repo (save_to_ref save)

read_last_save :: Git.Repo -> IO (Maybe SavePoint)
read_last_save repo = maybe (return Nothing) (fmap Just . ref_to_save)
    =<< Git.read_symbolic_ref repo "last-save"

find_next_save :: Git.Repo -> SavePoint -> IO SavePoint
find_next_save repo save =
    from_just =<< findM save_free (increment save : iterate split save)
    where
    save_free save = Maybe.isNothing <$> Git.read_ref repo (save_to_ref save)
    from_just = maybe -- This should never happen since iterate tries forever.
        (Git.throw $ "couldn't find a free save name after " ++ show save)
        return
    split (SavePoint xs) = SavePoint (0 : xs)
    increment (SavePoint []) = SavePoint [0]
    increment (SavePoint (x:xs)) = SavePoint (x + 1 : xs)

ref_to_save :: Git.Ref -> IO SavePoint
ref_to_save ref
    | not had_tags = Git.throw $
        "save ref must be in tags/: " ++ show ref
    | not (all (all Char.isDigit) versions) = Git.throw $
        "save ref must be ints separated by dots: " ++ show ref
    | otherwise = return $ SavePoint (reverse (map read versions))
    where
    (save, had_tags) = Seq.drop_prefix "tags/" ref
    versions = Seq.split "." save

save_to_ref :: SavePoint -> Git.Ref
save_to_ref (SavePoint versions) =
    "tags" </> Seq.join "." (map show (reverse versions))


-- * checkpoint

-- | State, updates to reach this state from the previous one (hack, should go
-- away), names of the cmds that produced this state.
--
-- Make updates go away by calculating them myself.  I know the modified tracks
-- from git, so I need only diff those.  I don't want to write them because
-- they duplicate all the stuff I'm writing.

-- | incremental save
checkpoint :: Git.Repo -> SaveHistory -> IO (Either String Git.Commit)
checkpoint repo hist@(History state updates names) = try_e "checkpoint" $ do
    Git.init repo
    -- If this is a new repo then do a save instead.
    last_commit <- Git.read_head_commit repo
    case last_commit of
        Nothing -> Right . fst <$> do_save repo hist
        Just last_commit -> do
            -- TODO event saving implemented but not loading
            let (errs, mods) = dump_diff False state updates
            if not (null errs) then return (Left (Seq.join ", " errs)) else do
            last_tree <- Git.commit_tree <$> Git.read_commit repo last_commit
            tree <- Git.modify_dir repo last_tree mods
            commit <- commit_tree repo tree $ unparse_names "checkpoint" names
            return $ Right commit

commit_tree :: Git.Repo -> Git.Tree -> String -> IO Git.Commit
commit_tree repo tree desc = do
    maybe_head <- Git.read_head_commit repo
    commit <- Git.write_commit repo Config.name Config.email
        (maybe [] (:[]) maybe_head) tree desc
    Git.update_head repo commit
    return commit

-- | This will tend to create redundant files, e.g. a block will be written
-- twice if two updates occur on it.  But 'Git.modify_dir' will filter out the
-- extras.
dump_diff :: Bool -> State.State -> [Update.UiUpdate]
    -> ([String], [Git.Modification])
dump_diff track_dir state =
    first (filter (not.null)) . Seq.partition_either . map mk
    where
    mk u@(Update.ViewUpdate view_id update) = case update of
        Update.DestroyView -> Right $ Git.Remove (id_to_path view_id)
        Update.BringToFront -> Left ""
        _ | Just view <- Map.lookup view_id (State.state_views state) ->
            Right $ Git.Add (id_to_path view_id) (Serialize.encode view)
        _ -> Left $ "update for nonexistent view_id: " ++ show u
    mk u@(Update.BlockUpdate block_id _)
        | Just block <- Map.lookup block_id (State.state_blocks state) =
            Right $ Git.Add (id_to_path block_id) (Serialize.encode block)
        | otherwise = Left $ "update for nonexistent block_id: " ++ show u
    mk u@(Update.TrackUpdate track_id update)
        | Just track <- Map.lookup track_id (State.state_tracks state) =
            case update of
                Update.TrackEvents start end | track_dir ->
                    Right $ dump_events state track_id start end
                _ -> Right $
                    Git.Add (id_to_path track_id) (Serialize.encode track)
        | otherwise = Left $ "update for nonexistent track_id: " ++ show u
    mk (Update.RulerUpdate ruler_id ruler) =
        Right $ Git.Add (id_to_path ruler_id) (Serialize.encode ruler)
    mk (Update.StateUpdate update) = case update of
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

class Ident id where id_to_path :: id -> FilePath
instance Ident ViewId where id_to_path = ("views" </>) . Id.ident_string
instance Ident BlockId where id_to_path = ("blocks" </>) . Id.ident_string
instance Ident TrackId where id_to_path = ("tracks" </>) . Id.ident_string
instance Ident RulerId where id_to_path = ("rulers" </>) . Id.ident_string

path_to_ident :: (Id.Id -> id) -> FilePath -> FilePath -> Either String id
path_to_ident mkid ns name = do
    ns <- if ns == "*GLOBAL*" then return Id.global_namespace
        else maybe (Left $ "invalid namespace: " ++ show ns) Right
            (Id.namespace ns)
    mkid <$> maybe
        (Left $ "invalid ident name: " ++ show name) Right (Id.id ns name)

-- ** events update

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
    -> IO (Either String (Maybe (LoadHistory, Git.Commit)))
load_previous_history repo state commit = try_e "load_previous_history" $ do
    commit_data <- Git.read_commit repo commit
    case Seq.head (Git.commit_parents commit_data) of
        Nothing -> return $ Right Nothing
        Just parent -> load_history repo state commit parent

-- | Try to a commits that has this one as a parent.
load_next_history :: Git.Repo -> State.State -> Git.Commit
    -> IO (Either String (Maybe (LoadHistory, Git.Commit)))
load_next_history repo state commit = try_e "load_next_history" $ do
    -- This won't work if I loaded something off-head.  In that case, I need
    -- the checkpoint I started from so I can start from there instead of HEAD.
    commits <- Git.read_log_head repo
    putStrLn $ "search in: " ++ show commit ++ " -> "
        ++ show (find_before commit commits)
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
    -> IO (Either String (Maybe (LoadHistory, Git.Commit)))
load_history repo state from_commit to_commit = do
    names <- parse_names . Git.commit_text
        =<< Git.read_commit repo to_commit
    result <- load_from repo from_commit (Just to_commit) state
    case result of
        Left err -> return $ Left err
        Right (new_state, cmd_updates) -> return $ Right $ Just
            (History new_state cmd_updates names, to_commit)

parse_names :: String -> IO [String]
parse_names text = case lines text of
    [_, names] -> readIO names
    _ -> error $ "can't parse description: " ++ show text

unparse_names :: String -> [String] -> String
unparse_names msg names = msg ++ "\n" ++ show names ++ "\n"

load_from :: Git.Repo -> Git.Commit -> Maybe Git.Commit -> State.State
    -> IO (Either String (State.State, [Update.CmdUpdate]))
load_from repo commit_from maybe_commit_to state = do
    commit_to <- default_head repo maybe_commit_to
    mods <- Git.diff_commits repo commit_from commit_to
    return $ undump_diff state mods

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
            ident <- path_to_ident mkid ns name
            vals <- delete_key ident (lens #$ state)
            return ((lens #= vals) state, updates)
    apply (state, updates) (Git.Add path bytes) = case split path of
        ["views", ns, name] -> add ns name Types.ViewId State.views
        ["blocks", ns, name] -> add ns name Types.BlockId State.blocks
        ["tracks", ns, name] -> do
            (state_to, updates) <- add ns name Types.TrackId State.tracks
            tid <- path_to_ident Types.TrackId ns name
            -- I don't save the CmdUpdates with the checkpoint, so to avoid
            -- having to rederive the entire track I do a little mini-diff
            -- just on the track.  It shouldn't be too expensive because it's
            -- only on one track at a time.
            let event_updates = Diff.track_diff state state_to tid
            return (state_to, event_updates ++ updates)
        ["rulers", ns, name] -> add ns name Types.RulerId State.rulers
        ["config"] -> do
            val <- decode path bytes
            return ((State.config #= val) state, updates)
        _ -> Left $ "unknown file modified: " ++ show path
        where
        add ns name mkid lens = do
            ident <- path_to_ident mkid ns name
            val <- decode path bytes
            return ((lens %= Map.insert ident val) state, updates)
    split = FilePath.splitDirectories

default_head :: Git.Repo -> Maybe Git.Commit -> IO Git.Commit
default_head _ (Just commit) = return commit
default_head repo Nothing =
    maybe (Git.throw $ "repo with no HEAD commit: " ++ show repo)
        return =<< Git.read_head_commit repo


-- * implementation

dump :: State.State -> Git.Dir
dump (State.State views blocks tracks rulers config) = Map.fromList
    [ ("views", Git.Dir $ dump_map views)
    , ("blocks", Git.Dir $ dump_map blocks)
    , ("tracks", Git.Dir $ dump_map tracks)
    , ("rulers", Git.Dir $ dump_map rulers)
    , ("config", Git.File $ Serialize.encode config)
    ]

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
    (Id.Id -> id) -> Map.Map Git.Name Git.File -> Either String (Map.Map id a)
undump_map mkid dir =
    Map.fromList . concat <$> mapM dir_subs (Map.toAscList dir)
    where
    dir_subs (name, Git.File _) =
        Left $ "expected dir but got file: " ++ show name
    dir_subs (name, Git.Dir subs) = do
        ns <- if name == "*GLOBAL*" then return Id.global_namespace
            else maybe (Left $ "invalid namespace: " ++ show name) return
                (Id.namespace name)
        mapM (undump_file ns) (Map.toList subs)
    undump_file _ (name, Git.Dir _) =
        Left $ "expected file but got dir: " ++ show name
    undump_file ns (name, Git.File bytes) = do
        ident <- maybe (Left $ "invalid name: " ++ show name) return $
            Id.id ns name
        val <- decode name bytes
        return (mkid ident, val)

dump_map :: (Id.Ident id, Serialize.Serialize a) => Map.Map id a -> Git.Dir
dump_map m = Map.fromList $ do
    (ns, id_elems) <- keyed_group ns_of (Map.toAscList m)
    return (ns, Git.Dir $ files_of id_elems)
    where
    ns_of = deglobal . Id.ident_namespace . fst
    deglobal ns
        | ns == Id.global_namespace = "*GLOBAL*"
        | otherwise = Id.un_namespace ns
    files_of id_elems = Map.fromList $ zip (map (Id.ident_name . fst) id_elems)
        (map (Git.File . Serialize.encode . snd) id_elems)

keyed_group :: (Ord key) => (a -> key) -> [a] -> [(key, [a])]
keyed_group key = map (\gs -> (key (head gs), gs)) . Seq.group key


-- * util

try :: String -> IO a -> IO (Either String a)
try caller op = do
    result <- Exception.try op
    return $ case result of
        Left (Git.GitException err) -> Left $ caller ++ ": " ++ err
        Right val -> Right val

try_e :: String -> IO (Either String a) -> IO (Either String a)
try_e caller op = do
    result <- Exception.try op
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
