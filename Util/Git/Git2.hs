{-# LANGUAGE OverloadedStrings #-}
-- | Low level interaction with the git object store, implemented via a libgit2
-- binding.
module Util.Git.Git2 where
import Prelude hiding (init)
import qualified Control.Exception as Exception
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as ByteString.Unsafe

import Foreign
import Foreign.C
import qualified System.Directory as Directory
import System.FilePath ((</>))

import Util.Control
import qualified Util.Git.LibGit2 as G


newtype Blob = Blob G.OID deriving (Eq, Show)
newtype Tree = Tree G.OID deriving (Eq, Show)
newtype Commit = Commit G.OID deriving (Eq, Show)

-- instance Pretty.Pretty Blob where pretty (Blob hash) = unparse_hash hash
-- instance Pretty.Pretty Tree where pretty (Tree hash) = unparse_hash hash
-- instance Pretty.Pretty Commit where pretty (Commit hash) = unparse_hash hash

type Repo = FilePath
-- | Repo-internal path.  Should not contain slashes.
type Name = FilePath
-- | This has the initial refs/ stripped off.
type Ref = FilePath

-- | Pointer to git_repository C type.
type RepoP = G.Repo

-- | True if it already existed.
init :: Repo -> IO Bool
init repo = ifM is_git (return True) $ do
    alloca $ \repopp -> withCString repo $ \pathp -> do
        G.check ("init " ++ repo) $ G.c'git_repository_init repopp pathp 1
        repop <- peek repopp
        G.c'git_repository_free repop
    return False
    where
    is_git = andM [Directory.doesDirectoryExist repo,
        Directory.doesDirectoryExist (repo </> "objects")]

with_repo :: Repo -> (RepoP -> IO a) -> IO a
with_repo path action = withCString path $ \pathp -> alloca $ \repopp -> do
    G.check ("open " ++ path) $ G.c'git_repository_open repopp pathp
    repop <- peek repopp
    result <- Exception.try $ action repop
    G.c'git_repository_free repop
    case result of
        Right ok -> return ok
        Left (G.GitException err) ->
            G.throw $ "repo " ++ show path ++ ": " ++ err

write_blob :: Repo -> ByteString -> IO Blob
write_blob repo bytes = with_repo repo $ \repop -> alloca $ \oidp ->
    ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(bytesp, len) -> do
        G.check "write_blob" $
            G.c'git_blob_create_frombuffer oidp repop bytesp (fromIntegral len)
        Blob <$> peek oidp

read_blob :: Repo -> Blob -> IO ByteString
read_blob repo (Blob blob) =
    with_repo repo $ \repop -> with blob $ \oidp -> alloca $ \blobpp -> do
        G.check ("blob_lookup: " ++ show blob) $
            G.c'git_blob_lookup blobpp repop oidp
        blobp <- peek blobpp
        bufp <- G.c'git_blob_rawcontent blobp
        len <- G.c'git_blob_rawsize blobp
        bytes <- ByteString.packCStringLen (bufp, fromIntegral len)
        G.c'git_blob_free blobp
        return bytes

write_tree :: Repo -> [(Name, Either Blob Tree)] -> IO Tree
write_tree repo files = with_repo repo $ \repop -> alloca $ \builderpp -> do
    G.check "treebuilder_create" $ G.c'git_treebuilder_create builderpp nullPtr
    builderp <- peek builderpp
    mapM_ (add builderp) files
    oid <- alloca $ \oidp -> do
        G.check "treebuilder_write" $
            G.c'git_treebuilder_write oidp repop builderp
        peek oidp
    G.c'git_treebuilder_free builderp
    return $ Tree oid
    where
    add builderp (name, Left (Blob oid)) = withCString name $ \namep ->
        with oid $ \oidp ->
            G.c'git_treebuilder_insert nullPtr builderp namep oidp 0o100644
    add builderp (name, Right (Tree oid)) = withCString name $ \namep ->
        with oid $ \oidp ->
            G.c'git_treebuilder_insert nullPtr builderp namep oidp 0o040000

read_tree :: Repo -> Tree -> IO [(Name, Either Blob Tree)]
read_tree repo tree = with_repo repo $ \repop ->
    with_tree repop tree $ \treep -> do
        count <- G.c'git_tree_entrycount treep
        entries <- mapM (G.c'git_tree_entry_byindex treep) [0..count-1]
        mapM peek_entry entries
    where
    peek_entry entryp = do
        typ <- G.c'git_tree_entry_type entryp
        name <- peekCString =<< G.c'git_tree_entry_name entryp
        oid <- peek =<< G.c'git_tree_entry_id entryp
        val <- if typ == G.gitObjTree then return (Right (Tree oid)) else
            if typ == G.gitObjBlob then return (Left (Blob oid)) else
            G.throw $ show oid ++ " expected tree or blob: " ++ show typ
        return (name, val)

with_tree :: G.Repo -> Tree -> (Ptr G.C'git_tree -> IO a) -> IO a
with_tree repop (Tree oid) io = with oid $ \oidp -> alloca $ \treepp -> do
    G.check ("tree_lookup: " ++ show oid) $
        G.c'git_tree_lookup treepp repop oidp
    treep <- peek treepp
    io treep `Exception.finally` G.c'git_tree_free treep

write_commit :: Repo -> String -> String -> [Commit] -> Tree -> String
    -> IO Commit
write_commit repo user email parents tree description =
    with_repo repo $ \repop -> with_sig $ \sigp ->
    with_commits repop parents $ \parents_len parentsp -> do
    with_tree repop tree $ \treep -> withCString description $ \descp -> do
    withCString "HEAD" $ \headp -> alloca $ \commitp -> do
        G.check "write_commit" $ G.c'git_commit_create commitp repop headp
            sigp sigp nullPtr descp treep (fromIntegral parents_len) parentsp
        Commit <$> peek commitp
    where
    with_sig io = withCString user $ \userp -> withCString email $ \emailp ->
        alloca $ \sigpp -> do
            G.check "signature_now" $ G.c'git_signature_now sigpp userp emailp
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
    with_repo repo $ \repop -> with_commit repop commit $ \commitp -> do
        author <- peek_user =<< peek =<< G.c'git_commit_author commitp
        tree <- peek =<< G.c'git_commit_tree_oid commitp
        parents_len <- G.c'git_commit_parentcount commitp
        parents <- mapM peek
            =<< mapM (G.c'git_commit_parent_oid commitp)
                (if parents_len == 0 then [] else [0..parents_len-1])
        desc <- peekCString =<< G.c'git_commit_message commitp
        return $ CommitData (Tree tree) (map Commit parents) author desc
    where
    peek_user sig = do
        name <- peekCString (G.c'git_signature'name sig)
        email <- peekCString (G.c'git_signature'email sig)
        return $ name ++ " <" ++ email ++ ">"

with_commit :: G.Repo -> Commit -> (Ptr G.C'git_commit -> IO a) -> IO a
with_commit repop (Commit oid) io = with oid $ \oidp -> alloca $ \commitpp -> do
    G.check ("tree_lookup: " ++ show oid) $
        G.c'git_commit_lookup commitpp repop oidp
    commitp <- peek commitpp
    io commitp `Exception.finally` G.c'git_commit_free commitp

{-
-- | Technically it's diff trees, but I always want to diff commits.
diff_commits :: Repo -> Commit -> Commit -> IO [Modification]
diff_commits repo (Commit c1) (Commit c2) = do
    output <- git repo ["diff-tree", "--no-renames", "-r",
        unparse_hash c1, unparse_hash c2] ""
    mapM parse (Char8.lines output)
    where
    parse line = case Char8.words line of
        [_, _, _, to_hash, status, path]
            | status == "D" -> return $ Remove (Char8.unpack path)
            | status == "M" || status == "A" -> do
                bytes <- read_blob repo (Blob to_hash)
                return $ Add (Char8.unpack path) bytes
            | otherwise ->
                throw $ "diff_commits: unknown status: " ++ show status
        _ -> throw $ "diff_commits: unparseable line: " ++ show line

-- | Get commits in reverse chronological order from the given ref.
read_log :: Repo -> Ref -> IO [Commit]
read_log repo ref = map (Commit . parse_hash) . Char8.lines <$>
    git repo ["rev-list", "refs" </> ref] ""

-- | Get commits in reverse chronological order from the HEAD.
read_log_head :: Repo -> IO [Commit]
read_log_head repo = map (Commit . parse_hash) . Char8.lines <$>
    git repo ["rev-list", "HEAD"] ""

gc :: Repo -> IO ()
gc repo = void $ git repo ["gc", "--aggressive"] ""

-- ** refs

write_ref :: Repo -> Commit -> Ref -> IO ()
write_ref repo (Commit commit) ref = do
    void $ git repo ["update-ref", "refs" </> ref, unparse_hash commit] ""

read_ref :: Repo -> Ref -> IO (Maybe Commit)
read_ref repo ref =
    (Just . Commit . parse_hash <$>
        git repo ["show-ref", "--verify", "--hash", "refs" </> ref] "")
        `Exception.catch` (\(_exc :: GitException) -> return Nothing)

write_symbolic_ref :: Repo -> Name -> Ref -> IO ()
write_symbolic_ref repo name ref =
    void $ git repo ["symbolic-ref", name, "refs" </> ref] ""

read_symbolic_ref :: Repo -> Name -> IO (Maybe Ref)
read_symbolic_ref repo name =
    ifM (Directory.doesFileExist (repo </> name))
        (Just . deref <$> git repo ["symbolic-ref", name] "")
        (return Nothing)
    where
    deref = drop_refs . Seq.strip . Char8.unpack
    drop_refs = drop 1 . dropWhile (/='/')

-- ** HEAD

-- | Point HEAD to a commit.
update_head :: Repo -> Commit -> IO ()
update_head repo (Commit commit) =
    void $ git repo ["update-ref", "HEAD", unparse_hash commit] ""

read_head_commit :: Repo -> IO (Maybe Commit)
read_head_commit repo = read_ref repo =<< read_head repo

write_head :: Repo -> Ref -> IO ()
write_head repo = write_symbolic_ref repo "HEAD"

read_head :: Repo -> IO Ref
read_head repo = maybe (throw "HEAD symbolic ref missing") return =<<
    read_symbolic_ref repo "HEAD"


-- * higher level

type Dir = Map.Map Name File
data File = File ByteString | Dir Dir deriving (Eq, Show)

make_dir :: [(FilePath, ByteString)] -> Either String Dir
make_dir = foldM merge Map.empty
    where
    merge dir (path, bytes) = insert dir (split path) bytes
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
    split = dropWhile (=="/") . FilePath.splitDirectories

write_dir :: Repo -> Dir -> IO Tree
write_dir repo filemap = do
    let files = Map.toList filemap
    hashes <- mapM (write . snd) files
    write_tree repo (zip (map fst files) hashes)
    where
    write (File bytes) = Left <$> write_blob repo bytes
    write (Dir dir) = Right <$> write_dir repo dir

read_dir  :: Repo -> Tree -> IO Dir
read_dir repo tree = do
    (names, files) <- unzip <$> read_tree repo tree
    files <- mapM read files
    return $ Map.fromList $ zip names files
    where
    read (Left blob) = File <$> read_blob repo blob
    read (Right tree) = Dir <$> read_dir repo tree

data Modification = Remove FilePath | Add FilePath ByteString
    deriving (Show)

instance Pretty.Pretty Modification where
    pretty (Remove fn) = "rm " ++ fn
    pretty (Add fn bytes) =
        "add " ++ fn ++ "{" ++ show (Char8.length bytes) ++ "}"

modify_dir :: Repo -> Tree -> [Modification] -> IO Tree
modify_dir repo (Tree tree) mods = do
    git repo ["read-tree", "--empty"] ""
    git repo ["read-tree", unparse_hash tree] ""
    mods <- forM (strip mods) $ \(path, maybe_bytes) -> do
        blob <- case maybe_bytes of
            Nothing -> return Nothing
            Just bytes -> Just <$> write_blob repo bytes
        return (path, blob)
    git repo ["update-index", "--replace", "--index-info"]
        (Char8.unlines (map mkline mods))
    Tree . parse_hash <$> git repo ["write-tree"] ""
    where
    mkline (path, Nothing) =
        "0 0000000000000000000000000000000000000000\t" <> UTF8.fromString path
    mkline (path, Just (Blob hash)) =
        "100644 " <> hash <> "\t" <> UTF8.fromString path
    -- Strip out redundent modifications.
    strip = Map.toList . Map.fromList . map extract
    extract (Remove fn) = (fn, Nothing)
    extract (Add fn bytes) = (fn, Just bytes)

-}
