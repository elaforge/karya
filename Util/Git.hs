-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
-- | Higher level wrapper around the hlibgit2 binding to libgit2.
module Util.Git (
    initialize
    -- * OID
    , Blob, Tree
    , Repo, FileName, Ref
    -- * repo
    , init
    -- * blob
    , write_blob, read_blob
    -- * tree
    , Modification(..), ModifyDir, ModifyFile(..), modifications_to_dir
    , write_tree, modify_tree, read_tree
    -- * commit
    , CommitData(..)
    , parse_commit, write_commit, read_commit
    -- * diff
    , diff_commits, diff_trees
    -- * refs
    , write_ref, read_ref
    -- ** symbolic refs
    , read_refs, read_ref_map
    , write_symbolic_ref, read_symbolic_ref
    -- ** HEAD ref
    , update_head, read_head_commit, write_head, read_head
    -- * revwalk
    , read_log, read_log_from, read_log_head
    -- * dir
    , Dir, File(..)
    , make_dir, flatten_dir
    , write_dir, read_dir
    -- * misc
    , gc
    -- * errors
    , throw, GitException(..)
) where
import           Prelude hiding (init)
import qualified Bindings.Libgit2 as G
import qualified Control.Exception as Exception
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import qualified Data.Char as Char
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Encoding
import qualified Data.Typeable as Typeable

import qualified System.Directory as Directory
import qualified System.Exit as Exit
import           System.FilePath ((</>))

import           Util.GitT (Commit(..), Repo)
import qualified Util.Lists as Lists
import qualified Util.Processes as Processes
import qualified Util.Strings as Strings

import           Foreign
import           Foreign.C
import           Global


-- | The library has to be initialized before you call any functions.
initialize :: IO a -> IO a
initialize =
    Exception.bracket_ (check "git_threads_init" G.c'git_threads_init)
        G.c'git_threads_shutdown

-- * OID

-- | This should be a bytestring of length 40 containing the git object ID.
newtype OID = OID ByteString deriving (Eq, Ord, Show)

with_oid :: OID -> (Ptr G.C'git_oid -> IO a) -> IO a
with_oid (OID bytes) action =
    ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(bytesp, len) ->
        alloca $ \oidp -> do
            check ("OID poke " ++ show bytes) $
                G.c'git_oid_fromstrn oidp bytesp (fromIntegral len)
            action oidp

peek_oid :: Ptr G.C'git_oid -> IO OID
peek_oid oidp = allocaBytes 40 $ \bufferp -> do
    G.c'git_oid_fmt bufferp oidp
    OID `fmap` Char8.packCStringLen (bufferp, 40)

cuchar_to_oid :: [CUChar] -> IO OID
cuchar_to_oid bytes = alloca $ \oidp -> do
    let bs = ByteString.pack (map fromIntegral bytes)
    ByteString.Unsafe.unsafeUseAsCString bs $ \bytesp -> do
        G.c'git_oid_fromraw oidp (castPtr bytesp)
        peek_oid oidp

newtype Blob = Blob OID deriving (Eq, Ord, Show)
newtype Tree = Tree OID deriving (Eq, Ord, Show)

instance Pretty Blob where
    pretty (Blob (OID oid)) = txt $ Char8.unpack oid
instance Pretty Tree where
    pretty (Tree (OID oid)) = txt $ Char8.unpack oid

-- | Repo-internal path.
type FileName = FilePath
-- | This has the initial refs/ stripped off.
type Ref = FilePath

-- | Pointer to git_repository C type.
type RepoP = Ptr G.C'git_repository

to_commit :: OID -> Commit
to_commit (OID bs) = Commit bs

from_commit :: Commit -> OID
from_commit (Commit bs) = OID bs

-- * repo

-- | True if it already existed.
init :: Repo -> IO Bool
init repo = ifM is_git (return True) $ do
    alloca $ \repopp -> withCString repo $ \pathp -> do
        check ("init " ++ repo) $ G.c'git_repository_init repopp pathp 1
        repop <- peek repopp
        G.c'git_repository_free repop
    return False
    where
    is_git = andM [Directory.doesDirectoryExist repo,
        Directory.doesDirectoryExist (repo </> "objects")]

with_repo :: Repo -> (RepoP -> IO a) -> IO a
with_repo path action = withCString path $ \pathp -> alloca $ \repopp -> do
    check ("open " ++ path) $ G.c'git_repository_open repopp pathp
    repop <- peek repopp
    result <- Exception.try $ action repop
    G.c'git_repository_free repop
    case result of
        Right ok -> return ok
        Left (GitException err) ->
            throw $ "repo " ++ show path ++ ": " ++ err

-- * blob

write_blob :: Repo -> ByteString -> IO Blob
write_blob repo bytes = with_repo repo $ \repop -> write_blob_repo repop bytes

write_blob_repo :: RepoP -> ByteString -> IO Blob
write_blob_repo repop bytes = alloca $ \oidp ->
    ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(bytesp, len) -> do
        check "write_blob" $
            G.c'git_blob_create_frombuffer oidp repop (castPtr bytesp)
                (fromIntegral len)
        Blob <$> peek_oid oidp

read_blob :: Repo -> Blob -> IO ByteString
read_blob repo blob = with_repo repo $ \repop -> read_blob_repo repop blob

read_blob_repo :: RepoP -> Blob -> IO ByteString
read_blob_repo repop (Blob blob) =
    with_oid blob $ \oidp -> alloca $ \blobpp -> do
        check ("blob_lookup: " ++ show blob) $
            G.c'git_blob_lookup blobpp repop oidp
        blobp <- peek blobpp
        bufp <- G.c'git_blob_rawcontent blobp
        len <- G.c'git_blob_rawsize blobp
        bytes <- Char8.packCStringLen (castPtr bufp, fromIntegral len)
        G.c'git_blob_free blobp
        return bytes

-- * tree

-- | Add fname Nothing means add a directory.
data Modification = Remove FilePath | Add FilePath ByteString
    deriving (Eq, Show)

instance Pretty Modification where
    pretty (Remove fn) = "rm " <> txt fn
    pretty (Add fn bytes) = "add" <> txt fn
        <> " {" <> showt (Char8.length bytes) <> "}"

type ModifyDir = [(FileName, ModifyFile)]
data ModifyFile = ModifyFile (Maybe ByteString) | ModifyDir ModifyDir
    deriving (Eq, Show)

modifications_to_dir :: [Modification] -> ModifyDir
modifications_to_dir mods = go (strip mods)
    where
    go entries = concatMap make (by_dir entries)
    make (dir, entries) = dirs ++ files
        where
        dirs = if null dir_ents then [] else [(dir, ModifyDir (go dir_ents))]
        files = case Lists.last file_ents of
            Nothing -> []
            Just (_, bytes) -> [(dir, ModifyFile bytes)]
        (file_ents, dir_ents) = List.partition (null . fst) entries
    by_dir entries = [(dir, map (first drop_dir) subs)
        | (dir, subs) <- Lists.keyedGroupSort (takeWhile (/='/') . fst) entries]
    drop_dir = dropWhile (=='/') . dropWhile (/='/')
    -- Strip out redundent modifications.
    strip = Map.toList . Map.fromList . map extract
    extract (Remove fn) = (fn, Nothing)
    extract (Add fn bytes) = (fn, Just bytes)

write_tree :: Repo -> Maybe Tree -> [(FileName, Maybe (Either Blob Tree))]
    -> IO Tree
write_tree repo maybe_from files = with_repo repo $ \repop ->
    maybe_with repop $ \fromp -> alloca $ \builderpp -> do
        check "treebuilder_create" $ G.c'git_treebuilder_create builderpp
            fromp
        builderp <- peek builderpp
        mapM_ (modify builderp) files
        oid <- alloca $ \oidp -> do
            check "treebuilder_write" $
                G.c'git_treebuilder_write oidp repop builderp
            peek_oid oidp
        G.c'git_treebuilder_free builderp
        return $ Tree oid
    where
    maybe_with repop io = case maybe_from of
        Just tree -> with_tree repop tree io
        Nothing -> io nullPtr
    modify builderp (name, val) = withCString name $ \namep -> case val of
        Nothing -> treebuilder_remove builderp name namep
        Just (Left blob) -> treebuilder_insert_file builderp name namep blob
        Just (Right tree) -> treebuilder_insert_dir builderp name namep tree

-- | Apply a list of modifications to an existing tree.
modify_tree :: Repo -> Tree -> [Modification] -> IO Tree
modify_tree repo tree mods = with_repo repo $ \repop ->
    go repop (Just tree) (modifications_to_dir mods)
    where
    go repop maybe_tree entries =
        with_maybe_tree repop maybe_tree $ \treep -> alloca $ \builderpp -> do
            check "treebuilder_create" $
                G.c'git_treebuilder_create builderpp treep
            builderp <- peek builderpp
            mapM_ (modify repop builderp) entries
            oid <- alloca $ \oidp -> do
                check "treebuilder_write" $
                    G.c'git_treebuilder_write oidp repop builderp
                peek_oid oidp
            G.c'git_treebuilder_free builderp
            return $ Tree oid
    with_maybe_tree _ Nothing io = io nullPtr
    with_maybe_tree repop (Just tree) io = with_tree repop tree io

    modify repop builderp (name, ModifyFile maybe_bytes) =
        withCString name $ \namep -> case maybe_bytes of
            Nothing -> treebuilder_remove builderp name namep
            Just bytes -> do
                blob <- write_blob_repo repop bytes
                treebuilder_insert_file builderp name namep blob
    modify repop builderp (name, ModifyDir entries) =
        withCString name $ \namep -> do
            entryp <- G.c'git_treebuilder_get builderp namep
            maybe_tree <- if entryp == nullPtr
                then return Nothing
                else Just . Tree <$> (peek_oid =<< G.c'git_tree_entry_id entryp)
            tree <- go repop maybe_tree entries
            -- Delete empty directories automatically.  This shouldn't be
            -- necessary, but without it git_diff_tree_to_tree gets extraneous
            -- Removes.
            ifM (empty_tree_repo repop tree)
                (treebuilder_remove builderp name namep)
                (treebuilder_insert_dir builderp name namep tree)

treebuilder_remove :: Ptr G.C'git_treebuilder -> String -> CString -> IO ()
treebuilder_remove builderp name namep =
    check ("git_treebuilder_remove: " ++ show name) $
        G.c'git_treebuilder_remove builderp namep

treebuilder_insert_file :: Ptr G.C'git_treebuilder -> String -> CString
    -> Blob -> IO ()
treebuilder_insert_file builderp name namep (Blob oid) =
    check ("git_treebuilder_insert: " ++ show name) $ with_oid oid $ \oidp ->
        G.c'git_treebuilder_insert nullPtr builderp namep oidp 0o100644

treebuilder_insert_dir :: Ptr G.C'git_treebuilder -> String -> CString
    -> Tree -> IO ()
treebuilder_insert_dir builderp name namep (Tree oid) =
    check ("git_treebuilder_insert: " ++ show (name ++ "/")) $
    with_oid oid $ \oidp ->
        G.c'git_treebuilder_insert nullPtr builderp namep oidp 0o040000

read_tree :: Repo -> Tree -> IO [(FileName, Either Blob Tree)]
read_tree repo tree = with_repo repo $ \repop ->
    with_tree repop tree $ \treep -> do
        count <- G.c'git_tree_entrycount treep
        entries <- mapM (G.c'git_tree_entry_byindex treep)
            (Lists.range' 0 count 1)
        mapM peek_entry entries
    where
    peek_entry entryp = do
        typ <- G.c'git_tree_entry_type entryp
        name <- peekCString =<< G.c'git_tree_entry_name entryp
        oid <- peek_oid =<< G.c'git_tree_entry_id entryp
        val <- if typ == G.c'GIT_OBJ_TREE then return (Right (Tree oid)) else
            if typ == G.c'GIT_OBJ_BLOB then return (Left (Blob oid)) else
            throw $ show oid ++ " expected tree or blob: " ++ show typ
        return (name, val)

empty_tree_repo :: RepoP -> Tree -> IO Bool
empty_tree_repo repop tree = with_tree repop tree $ \treep -> do
    count <- G.c'git_tree_entrycount treep
    return $ count == 0

with_tree :: RepoP -> Tree -> (Ptr G.C'git_tree -> IO a) -> IO a
with_tree repop (Tree oid) io = with_oid oid $ \oidp -> alloca $ \treepp -> do
    check ("tree_lookup: " ++ show oid) $
        G.c'git_tree_lookup treepp repop oidp
    treep <- peek treepp
    io treep `Exception.finally` G.c'git_tree_free treep

-- * commit

parse_commit :: String -> Maybe Commit
parse_commit str
    | length str == 40 = Just $ Commit $ Char8.pack str
    | otherwise = Nothing

write_commit :: Repo -> Text -> Text -> [Commit] -> Tree -> String
    -> IO Commit
write_commit repo user email parents tree description =
    with_repo repo $ \repop -> with_sig $ \sigp ->
    with_commits repop parents $ \parents_len parentsp ->
    with_tree repop tree $ \treep -> withCString description $ \descp ->
    withCString "HEAD" $ \headp -> alloca $ \commitp -> do
        check "write_commit" $ G.c'git_commit_create commitp repop headp
            sigp sigp nullPtr descp treep (fromIntegral parents_len) parentsp
        to_commit <$> peek_oid commitp
    where
    with_sig io = withText user $ \userp -> withText email $ \emailp ->
        alloca $ \sigpp -> do
            check "signature_now" $ G.c'git_signature_now sigpp userp emailp
            sigp <- peek sigpp
            io sigp `Exception.finally` G.c'git_signature_free sigp
    with_commits repop commits io = go [] commits $ \ps -> withArrayLen ps io
        where
        go ps [] cont = cont (reverse ps)
        go ps (c:cs) cont = with_commit repop c $ \p -> go (p:ps) cs cont

data CommitData = CommitData {
    commit_tree :: !Tree
    , commit_parents :: ![Commit]
    , commit_author :: !String
    , commit_text :: !String
    } deriving (Eq, Show)

read_commit :: Repo -> Commit -> IO CommitData
read_commit repo commit =
    with_repo repo $ \repop -> read_commit_repo repop commit

read_commit_repo :: RepoP -> Commit -> IO CommitData
read_commit_repo repop commit = with_commit repop commit $ \commitp -> do
    author <- peek_user =<< peek =<< G.c'git_commit_author commitp
    tree <- peek_oid =<< G.c'git_commit_tree_id commitp
    parents_len <- G.c'git_commit_parentcount commitp
    parents <- mapM peek_oid
        =<< mapM (G.c'git_commit_parent_id commitp)
            (if parents_len == 0 then [] else [0..parents_len-1])
    desc <- peekCString =<< G.c'git_commit_message commitp
    return $ CommitData (Tree tree) (map to_commit parents) author desc
    where
    peek_user sig = do
        name <- peekCString (G.c'git_signature'name sig)
        email <- peekCString (G.c'git_signature'email sig)
        return $ name ++ " <" ++ email ++ ">"

with_commit :: RepoP -> Commit -> (Ptr G.C'git_commit -> IO a) -> IO a
with_commit repop commit io = with_oid (from_commit commit) $
    \oidp -> alloca $ \commitpp -> do
        check ("tree_lookup: " ++ show commit) $
            G.c'git_commit_lookup commitpp repop oidp
        commitp <- peek commitpp
        io commitp `Exception.finally` G.c'git_commit_free commitp

-- * diff

diff_commits :: Repo -> Commit -> Commit -> IO [Modification]
diff_commits repo old new = with_repo repo $ \repop -> do
    oldc <- read_commit_repo repop old
    newc <- read_commit_repo repop new
    diff_trees repo (commit_tree oldc) (commit_tree newc)

-- | Recursively diff two trees.
diff_trees :: Repo -> Tree -> Tree -> IO [Modification]
diff_trees repo old new =
    with_repo repo $ \repop -> diff_tree_repo repop old new

diff_tree_repo :: RepoP -> Tree -> Tree -> IO [Modification]
diff_tree_repo repop old new =
    with_tree repop old $ \oldp -> with_tree repop new $ \newp -> do
    diffs <- alloca $ \listpp -> do
        check "diff_tree" $
            G.c'git_diff_tree_to_tree listpp repop oldp newp nullPtr
        listp <- peek listpp
        ref <- IORef.newIORef []
        -- with_fptr (G.mk'git_diff_line_cb (diff_cb ref)) $ \callback ->
        --     check "diff_print" $ G.c'git_diff_print listp
        --         G.c'GIT_DIFF_FORMAT_NAME_STATUS callback nullPtr
        with_fptr (G.mk'git_diff_data_cb (diff_cb ref)) $ \callback ->
            check "diff_print_compact" $
                G.c'git_diff_print_compact listp callback nullPtr
        IORef.readIORef ref
    concatMapM to_mod diffs
    where
    -- I'm only interested in which files were deleted, added, or modified.
    to_mod (status, path, oid)
        | status == G.c'GIT_DELTA_DELETED = return [Remove path]
        | status == G.c'GIT_DELTA_ADDED || status == G.c'GIT_DELTA_MODIFIED = do
            bytes <- read_blob_repo repop (Blob oid)
            return [Add path bytes]
        | otherwise = throw $ "diff_trees " ++ show (old, new)
            ++ ": unknown status: " ++ show status
    diff_cb ref deltap _rangep _line_origin _contentp _content_len _data = do
        delta <- peek deltap
        let new_file = G.c'git_diff_delta'new_file delta
            status = G.c'git_diff_delta'status delta
        let oid_bytes = G.c'git_oid'id (G.c'git_diff_file'oid new_file)
            pathp = G.c'git_diff_file'path new_file
        path <- peekCString pathp
        oid <- cuchar_to_oid oid_bytes
        IORef.modifyIORef ref ((status, path, oid):)
        return 0

    -- Used for libgit2 0.20
    -- diff_cb ref deltap _hunkp _linep _payloadp = do
    --     G.C'git_diff_delta status _flags _similarity _nfiles
    --         _old_file new_file <- peek deltap
    --     let G.C'git_diff_file oid pathp _size _flags _mode = new_file
    --     path <- peekCString pathp
    --     IORef.modifyIORef ref ((status, path, oid):)
    --     return 0

-- * refs

write_ref :: Repo -> Commit -> Ref -> IO ()
write_ref repo commit ref = with_repo repo $ \repop ->
    with_oid (from_commit commit) $ \commitp -> with_ref_name ref $ \namep ->
    alloca $ \refpp -> do
        check ("write_ref " ++ show ref) $
            G.c'git_reference_create refpp repop namep commitp 1
        refp <- peek refpp
        when (refp /= nullPtr) $
            G.c'git_reference_free refp

read_ref :: Repo -> Ref -> IO (Maybe Commit)
read_ref repo ref = with_repo repo $ \repop -> read_ref_repo repop ref

read_ref_repo :: RepoP -> Ref -> IO (Maybe Commit)
read_ref_repo repop ref = with_ref_name ref $ \namep ->
    alloca $ \oidp -> do
        code <- G.c'git_reference_name_to_id oidp repop namep
        if code /= G.c'GIT_OK then return Nothing else do
        oid <- peek_oid oidp
        return (Just (to_commit oid))

with_ref :: RepoP -> Ref -> (Ptr G.C'git_reference -> IO a) -> IO a
with_ref repop ref io =
    withCString ref $ \namep -> alloca $ \refpp -> do
        check ("reference_lookup " ++ show ref) $
            G.c'git_reference_lookup refpp repop namep
        refp <- peek refpp
        io refp `Exception.finally` when (refp /= nullPtr)
            (G.c'git_reference_free refp)

-- | Read all the refs in the repo.
read_refs :: RepoP -> IO [Ref]
read_refs repop = alloca $ \arrayp -> do
    check "read_refs" $ G.c'git_reference_list arrayp repop
        G.c'GIT_REF_LISTALL
    G.C'git_strarray stringsp count <- peek arrayp
    strps <- peekArray (fromIntegral count) stringsp
    refs <- mapM (peek_ref_name "") strps
    G.c'git_strarray_free arrayp
    return refs

-- | Read all refs along with their commits.
read_ref_map :: Repo -> IO (Map Ref Commit)
read_ref_map repo = with_repo repo $ \repop -> do
    refs <- read_refs repop
    commits <- mapM (read_ref_repo repop) refs
    return $ Map.fromList
        [(ref, commit) | (ref, Just commit) <- zip refs commits]

with_ref_name :: Ref -> (CString -> IO a) -> IO a
with_ref_name ref io = withCString ("refs" </> ref) $ \namep -> io namep

peek_ref_name :: String -> CString -> IO Ref
peek_ref_name prefix str = do
    (name, stripped) <- Lists.dropPrefix "refs/" <$> peekCString str
    unless stripped $ throw $ prefix ++ "wasn't in refs/: " ++ show name
    return name

-- ** symbolic refs

write_symbolic_ref :: Repo -> Ref -> Ref -> IO ()
write_symbolic_ref repo sym ref
    | not (valid_symbolic_ref sym) = throw $ "ref should be ALL_CAPS: " ++ sym
    | otherwise = with_repo repo $ \repop ->
        with_ref_name ref $ \namep -> withCString sym $ \symp ->
        alloca $ \refpp -> do
            refp <- check_lookup "reference_symbolic_create" refpp $
                G.c'git_reference_symbolic_create refpp repop symp namep 1
            when (refp /= nullPtr) $
                G.c'git_reference_free refp

read_symbolic_ref :: Repo -> Ref -> IO (Maybe Ref)
read_symbolic_ref repo sym = with_repo repo $ \repop ->
    with_ref repop sym $ \symp -> alloca $ \refpp -> do
        refp <- check_lookup "reference_resolve" refpp $
            G.c'git_reference_resolve refpp symp
        if refp == nullPtr then return Nothing
            else fmap Just $ peek_ref_name ("ref of " ++ show sym ++ " ")
                =<< G.c'git_reference_name refp

-- | Rationale documented at: https://github.com/libgit2/libgit2/pull/938
valid_symbolic_ref :: Ref -> Bool
valid_symbolic_ref ref =
    not (null ref) && all (\c -> Char.isAsciiUpper c || c == '_') ref
        && head ref /= '_' && last ref /= '_'

-- ** HEAD ref

-- | Point HEAD to a commit.
update_head :: Repo -> Commit -> IO ()
update_head repo commit = do
    maybe_ref <- read_symbolic_ref repo "HEAD"
    maybe (throw "HEAD symbolic ref missing") (write_ref repo commit) maybe_ref

read_head_commit :: Repo -> IO (Maybe Commit)
read_head_commit repo = read_ref repo =<< read_head repo

write_head :: Repo -> Ref -> IO ()
write_head repo = write_symbolic_ref repo "HEAD"

read_head :: Repo -> IO Ref
read_head repo = maybe (throw "HEAD symbolic ref missing") return =<<
    read_symbolic_ref repo "HEAD"

-- * revwalk

-- | Get commits in reverse chronological order from the given ref.
read_log :: Repo -> Ref -> IO [Commit]
read_log repo ref = with_repo repo $ \repop -> with_ref_name ref $ \refnamep ->
    with_revwalk repop $ \walkp -> do
        check ("revwalk_push_ref: " ++ ref) $
            G.c'git_revwalk_push_ref walkp refnamep
        walk walkp [SortTime]

-- | Read commits starting from the given commit.
read_log_from :: Repo -> Commit -> IO [Commit]
read_log_from repo commit = with_repo repo $ \repop ->
    with_oid (from_commit commit) $ \oidp -> with_revwalk repop $ \walkp -> do
        check ("revwalk_push: " ++ show commit) $
            G.c'git_revwalk_push walkp oidp
        walk walkp [SortTime]

-- | Get commits in reverse chronological order from the HEAD.
read_log_head :: Repo -> IO [Commit]
read_log_head repo = read_log repo =<< read_head repo

data SortFlag = SortTopological | SortTime | SortReverse deriving (Show)

walk :: Ptr G.C'git_revwalk -> [SortFlag] -> IO [Commit]
walk walkp flags = do
    G.c'git_revwalk_sorting walkp
        (foldl' (Bits..|.) git_sort_none (map flag flags))
    alloca $ \oidp -> while_just (next oidp)
    where
    next oidp = do
        errno <- G.c'git_revwalk_next oidp walkp
        if errno == G.c'GIT_ITEROVER then return Nothing else do
        check "revwalk_next" (return errno)
        oid <- peek_oid oidp
        return (Just (to_commit oid))
    while_just io = do
        maybe_val <- io
        case maybe_val of
            Nothing -> return []
            Just val -> do
                vals <- while_just io
                return (val : vals)
    flag f = case f of
        SortTopological -> git_sort_topological
        SortTime -> git_sort_time
        SortReverse -> git_sort_reverse

-- | These aren't declared as a type at the C level, so the binding doesn't
-- have them.
git_sort_none, git_sort_topological, git_sort_time, git_sort_reverse :: CUInt
git_sort_none = 0
git_sort_topological = 1
git_sort_time = 2
git_sort_reverse = 4

with_revwalk :: RepoP -> (Ptr G.C'git_revwalk -> IO a) -> IO a
with_revwalk repop io = alloca $ \walkpp -> do
    check "revwalk_new" $ G.c'git_revwalk_new walkpp repop
    walkp <- peek walkpp
    io walkp `Exception.finally` G.c'git_revwalk_free walkp

-- * dir

type Dir = Map FileName File
data File = File ByteString | Dir Dir deriving (Eq, Show)

make_dir :: [(FilePath, ByteString)] -> Either String Dir
make_dir = foldM merge Map.empty
    where
    -- System.FilePath is incorrect because git always uses /s.
    merge dir (path, bytes) = insert dir (Lists.split (=='/') path) bytes
    insert _ [] bytes = Left $ "can't insert into empty path: " ++ show bytes
    insert files [name] bytes = return $ Map.insert name (File bytes) files
    insert files (name : names) bytes = do
        subs <- case Map.lookup name files of
            Just (Dir subs) -> return subs
            Just (File _) ->
                Left $ "can't insert below a file: " ++ show name
            Nothing -> return Map.empty
        subs <- insert subs names bytes
        return $ Map.insert name (Dir subs) files

flatten_dir :: Dir -> [(FilePath, ByteString)]
flatten_dir = concatMap flatten . Map.toList
    where
    flatten (name, File bytes) = [(name, bytes)]
    flatten (name, Dir dir) = map (first (name </>)) (flatten_dir dir)

write_dir :: Repo -> Dir -> IO Tree
write_dir repo filemap = do
    let files = Map.toList filemap
    hashes <- mapM (write . snd) files
    write_tree repo Nothing (zip (map fst files) (map Just hashes))
    where
    write (File bytes) = Left <$> write_blob repo bytes
    write (Dir dir) = Right <$> write_dir repo dir

read_dir :: Repo -> Tree -> IO Dir
read_dir repo tree = do
    (names, files) <- unzip <$> read_tree repo tree
    files <- mapM read files
    return $ Map.fromList $ zip names files
    where
    read (Left blob) = File <$> read_blob repo blob
    read (Right tree) = Dir <$> read_dir repo tree

-- * misc

-- | GC the repo.  As far as I know, there's no way to do this with libgit2, so
-- it invokes the standalone git command.
gc :: Repo -> IO ()
gc repo = git repo ["gc", "--aggressive"] "" >> return ()

git :: Repo -> [String] -> ByteString -> IO ByteString
git repo = git_env repo []

git_env :: Repo -> [(String, String)] -> [String] -> ByteString
    -> IO ByteString
git_env repo env args stdin = do
    (ex, out, err) <- Processes.readProcessWithExitCode
        (Just (("GIT_DIR", repo) : env)) "git" args stdin
    -- let sin = " <" ++ show (Char8.length stdin)
    -- let sin = if Char8.null stdin then "" else " <" ++ show stdin
    -- putStrLn $ unwords ("git" : args) ++ sin ++ " ==> "
    --     ++ Strings.strip (Char8.unpack out)
    case ex of
        Exit.ExitFailure code -> throw $
            repo ++ " -- " ++ unwords ("git" : args) ++ ": " ++ show code
            ++ " " ++ Strings.strip (Char8.unpack err)
        _ -> return out

-- * error

type Error = CInt

error_msg :: Error -> Maybe String
error_msg errno
    | errno == G.c'GIT_OK = Nothing
    | otherwise = Just $ Map.findWithDefault
        ("undocumented errno: " ++ show errno) errno errors
    where
    errors = Map.fromList
        [ (G.c'GIT_ERROR, "error")
        , (G.c'GIT_ENOTFOUND, "not found")
        , (G.c'GIT_EEXISTS, "already exists")
        , (G.c'GIT_EAMBIGUOUS, "short oid is ambiguous")
        , (G.c'GIT_EBUFS, "undocumented GIT_EBUFS")
        , (G.c'GIT_PASSTHROUGH, "passthrough")
        , (G.c'GIT_ITEROVER, "iteration over")
        ]

newtype GitException = GitException String deriving (Typeable.Typeable)
instance Exception.Exception GitException
instance Show GitException where
    show (GitException msg) = "GitException: " ++ msg

throw :: String -> IO a
throw = Exception.throwIO . GitException

check :: String -> IO Error -> IO ()
check caller action = do
    errno <- action
    case error_msg errno of
        Nothing -> return ()
        Just msg -> throw $ caller ++ ": " ++ msg

check_lookup :: String -> Ptr (Ptr a) -> IO Error -> IO (Ptr a)
check_lookup caller ptrptr io = do
    errno <- io
    if errno == G.c'GIT_ENOTFOUND then return nullPtr else do
    case error_msg errno of
        Nothing -> peek ptrptr
        Just msg -> throw $ caller ++ ": " ++ msg

-- * util

with_fptr :: IO (FunPtr a) -> (FunPtr a -> IO b) -> IO b
with_fptr make io = do
    fptr <- make
    io fptr `Exception.finally` freeHaskellFunPtr fptr

withText :: Text -> (CString -> IO a) -> IO a
withText = ByteString.useAsCString . Encoding.encodeUtf8
