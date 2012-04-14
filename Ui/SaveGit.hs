{-# LANGUAGE PatternGuards #-}
module Ui.SaveGit where
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.Debug as Debug
import qualified Util.Git as Git
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import Cmd.Serialize ()
import Types


-- * save

-- | Like 'checkpoint', but add a tag too, and write compact tracks.
-- TODO what should the description be?
save :: FilePath -> State.State -> IO (Git.Commit, Save)
save repo state = do
    Git.init repo
    tree <- Git.write_dir repo (dump state)
    commit <- commit_tree repo tree "save\n"
    last_save <- read_last_save repo
    save <- find_next_save repo (Maybe.fromMaybe (Save []) last_save)
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
newtype Save = Save [Int] deriving (Eq, Show)

-- | Create a tag for the given commit, and point last-save at it.
write_save_ref :: Git.Repo -> Save -> Git.Commit -> IO ()
write_save_ref repo save commit = do
    let ref = save_to_ref save
    Git.write_ref repo commit ref
    Git.write_symbolic_ref repo "last-save" ref

read_save_ref :: Git.Repo -> Save -> IO (Maybe Git.Commit)
read_save_ref repo save = Git.read_ref repo (save_to_ref save)

read_last_save :: Git.Repo -> IO (Maybe Save)
read_last_save repo = maybe (return Nothing) (fmap Just . ref_to_save)
    =<< Git.read_symbolic_ref repo "last-save"

find_next_save :: Git.Repo -> Save -> IO Save
find_next_save repo save =
    from_just =<< findM save_free (increment save : iterate split save)
    where
    save_free save = Maybe.isNothing <$> Git.read_ref repo (save_to_ref save)
    from_just = maybe -- This should never happen since iterate tries forever.
        (Git.throw $ "couldn't find a free save name after " ++ show save)
        return
    split (Save xs) = Save (0 : xs)
    increment (Save []) = Save [0]
    increment (Save (x:xs)) = Save (x + 1 : xs)

ref_to_save :: Git.Ref -> IO Save
ref_to_save ref
    | not had_tags = Git.throw $
        "save ref must be in tags/: " ++ show ref
    | not (all (all Char.isDigit) versions) = Git.throw $
        "save ref must be ints separated by dots: " ++ show ref
    | otherwise = return $ Save (reverse (map read versions))
    where
    (save, had_tags) = Seq.drop_prefix "tags/" ref
    versions = Seq.split "." save

save_to_ref :: Save -> Git.Ref
save_to_ref (Save versions) =
    "tags" </> Seq.join "." (map show (reverse versions))


-- * checkpoint

-- | incremental save
checkpoint :: FilePath -> State.State -> [Update.CmdUpdate]
    -> IO (Either [String] Git.Commit)
checkpoint repo state updates = do
    Git.init repo
    -- If this is a new repo then do a save instead.
    last_commit <- Git.read_head_commit repo
    case last_commit of
        Nothing -> Right . fst <$> save repo state
        Just last_commit -> do
            let (errs, mods) = dump_diff state updates
            if not (null errs) then return (Left errs) else do
            last_tree <- Git.read_commit repo last_commit
            tree <- Git.modify_dir repo last_tree mods
            commit <- commit_tree repo tree "checkpoint\n"
            return $ Right commit

commit_tree :: Git.Repo -> Git.Tree -> String -> IO Git.Commit
commit_tree repo tree desc = do
    maybe_head <- Git.read_head_commit repo
    commit <- Git.write_commit repo "me" "email"
        (maybe [] (:[]) maybe_head) tree desc
    Git.update_head repo commit
    return commit

-- | This will tend to create redundant files, e.g. a block will be written
-- twice if two updates occur on it.  But 'Git.modify_dir' will filter out the
-- extras.
dump_diff :: State.State -> [Update.CmdUpdate] -> ([String], [Git.Modification])
dump_diff state = first (filter (not.null)) . Seq.partition_either . map mk
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
    -- TODO write event files if flag is set
    mk u@(Update.TrackUpdate track_id _)
        | Just track <- Map.lookup track_id (State.state_tracks state) =
            Right $ Git.Add (id_to_path track_id) (Serialize.encode track)
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


-- * load

load :: FilePath -> Maybe Git.Commit -> IO (Either String State.State)
load repo maybe_commit = do
    -- TODO have to handle both compact and expanded tracks
    commit <- default_head repo maybe_commit
    tree <- Git.read_commit repo commit
    dirs <- Git.read_dir repo tree
    return $ undump dirs

load_from :: FilePath -> Git.Commit -> Maybe Git.Commit -> State.State
    -> IO (Either String State.State)
load_from repo commit_from maybe_commit_to state = do
    commit_to <- default_head repo maybe_commit_to
    mods <- Git.diff_commits repo commit_from commit_to
    return $ undump_diff state mods

undump_diff :: State.State -> [Git.Modification] -> Either String State.State
undump_diff = foldM apply
    where
    apply state (Git.Remove path) = case FilePath.splitDirectories path of
        ["views", ns, name] -> delete ns name Types.ViewId State.views
        ["blocks", ns, name] -> delete ns name Types.BlockId State.blocks
        ["tracks", ns, name] -> delete ns name Types.TrackId State.tracks
        ["rulers", ns, name] -> delete ns name Types.RulerId State.rulers
        _ -> Left $ "unknown file deleted: " ++ show path
        where
        delete ns name mkid lens = do
            ident <- path_to_ident mkid ns name
            vals <- delete_key ident (lens $# state)
            return $ (lens =# vals) state
    apply state (Git.Add path bytes) = case FilePath.splitDirectories path of
        ["views", ns, name] -> add ns name Types.ViewId State.views
        ["blocks", ns, name] -> add ns name Types.BlockId State.blocks
        ["tracks", ns, name] -> add ns name Types.TrackId State.tracks
        ["rulers", ns, name] -> add ns name Types.RulerId State.rulers
        ["config"] -> do
            val <- decode path bytes
            return $ (State.config =# val) state
        _ -> Left $ "unknown file modified: " ++ show path
        where
        add ns name mkid lens = do
            ident <- path_to_ident mkid ns name
            val <- decode path bytes
            return $ (lens =% Map.insert ident val) state

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
    ns_of = deglobal . Id.id_namespace . Id.unpack_id . fst
    deglobal ns
        | ns == Id.global_namespace = "*GLOBAL*"
        | otherwise = Id.un_namespace ns
    files_of id_elems = Map.fromList $ zip (map (Id.ident_name . fst) id_elems)
        (map (Git.File . Serialize.encode . snd) id_elems)

keyed_group :: (Ord key) => (a -> key) -> [a] -> [(key, [a])]
keyed_group key = map (\gs -> (key (head gs), gs)) . Seq.group key


-- * util

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
