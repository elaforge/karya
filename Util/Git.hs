-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Higher level wrapper around the hlibgit2 binding to libgit2.
module Util.Git (
    initialize
    -- * OID
    , Blob, Tree
    , Repo, FileName, Ref
    -- * repo
    , init
    -- * blob
    , writeBlob, readBlob
    -- * tree
    , Modification(..), ModifyDir, ModifyFile(..), modificationsToDir
    , writeTree, modifyTree, readTree
    -- * commit
    , CommitData(..)
    , parseCommit, writeCommit, readCommit
    -- * diff
    , diffCommits, diffTrees
    -- * refs
    , writeRef, readRef
    -- ** symbolic refs
    , readRefs, readRefMap
    , writeSymbolicRef, readSymbolicRef
    -- ** HEAD ref
    , updateHead, readHeadCommit, writeHead, readHead
    -- * revwalk
    , readLog, readLogFrom, readLogHead
    -- * dir
    , Dir, File(..)
    , makeDir, flattenDir
    , writeDir, readDir
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

withOid :: OID -> (Ptr G.C'git_oid -> IO a) -> IO a
withOid (OID bytes) action =
    ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(bytesp, len) ->
        alloca $ \oidp -> do
            check ("OID poke " ++ show bytes) $
                G.c'git_oid_fromstrn oidp bytesp (fromIntegral len)
            action oidp

peekOid :: Ptr G.C'git_oid -> IO OID
peekOid oidp = allocaBytes 40 $ \bufferp -> do
    G.c'git_oid_fmt bufferp oidp
    OID `fmap` Char8.packCStringLen (bufferp, 40)

cucharToOid :: [CUChar] -> IO OID
cucharToOid bytes = alloca $ \oidp -> do
    let bs = ByteString.pack (map fromIntegral bytes)
    ByteString.Unsafe.unsafeUseAsCString bs $ \bytesp -> do
        G.c'git_oid_fromraw oidp (castPtr bytesp)
        peekOid oidp

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

toCommit :: OID -> Commit
toCommit (OID bs) = Commit bs

fromCommit :: Commit -> OID
fromCommit (Commit bs) = OID bs

-- * repo

-- | True if it already existed.
init :: Repo -> IO Bool
init repo = ifM isGit (pure True) $ do
    alloca $ \repopp -> withCString repo $ \pathp -> do
        check ("init " ++ repo) $ G.c'git_repository_init repopp pathp 1
        repop <- peek repopp
        G.c'git_repository_free repop
    pure False
    where
    isGit = andM [Directory.doesDirectoryExist repo,
        Directory.doesDirectoryExist (repo </> "objects")]

withRepo :: Repo -> (RepoP -> IO a) -> IO a
withRepo path action = withCString path $ \pathp -> alloca $ \repopp -> do
    check ("open " ++ path) $ G.c'git_repository_open repopp pathp
    repop <- peek repopp
    result <- Exception.try $ action repop
    G.c'git_repository_free repop
    case result of
        Right ok -> pure ok
        Left (GitException err) ->
            throw $ "repo " ++ show path ++ ": " ++ err

-- * blob

writeBlob :: Repo -> ByteString -> IO Blob
writeBlob repo bytes = withRepo repo $ \repop -> writeBlobRepo repop bytes

writeBlobRepo :: RepoP -> ByteString -> IO Blob
writeBlobRepo repop bytes = alloca $ \oidp ->
    ByteString.Unsafe.unsafeUseAsCStringLen bytes $ \(bytesp, len) -> do
        check "write_blob" $
            G.c'git_blob_create_frombuffer oidp repop (castPtr bytesp)
                (fromIntegral len)
        Blob <$> peekOid oidp

readBlob :: Repo -> Blob -> IO ByteString
readBlob repo blob = withRepo repo $ \repop -> readBlobRepo repop blob

readBlobRepo :: RepoP -> Blob -> IO ByteString
readBlobRepo repop (Blob blob) =
    withOid blob $ \oidp -> alloca $ \blobpp -> do
        check ("blob_lookup: " ++ show blob) $
            G.c'git_blob_lookup blobpp repop oidp
        blobp <- peek blobpp
        bufp <- G.c'git_blob_rawcontent blobp
        len <- G.c'git_blob_rawsize blobp
        bytes <- Char8.packCStringLen (castPtr bufp, fromIntegral len)
        G.c'git_blob_free blobp
        pure bytes

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

modificationsToDir :: [Modification] -> ModifyDir
modificationsToDir mods = go (strip mods)
    where
    go entries = concatMap make (byDir entries)
    make (dir, entries) = dirs ++ files
        where
        dirs = if null dirEnts then [] else [(dir, ModifyDir (go dirEnts))]
        files = case Lists.last fileEnts of
            Nothing -> []
            Just (_, bytes) -> [(dir, ModifyFile bytes)]
        (fileEnts, dirEnts) = List.partition (null . fst) entries
    byDir entries = [(dir, map (first dropDir) subs)
        | (dir, subs) <- Lists.keyedGroupSort (takeWhile (/='/') . fst) entries]
    dropDir = dropWhile (=='/') . dropWhile (/='/')
    -- Strip out redundent modifications.
    strip = Map.toList . Map.fromList . map extract
    extract (Remove fn) = (fn, Nothing)
    extract (Add fn bytes) = (fn, Just bytes)

writeTree :: Repo -> Maybe Tree -> [(FileName, Maybe (Either Blob Tree))]
    -> IO Tree
writeTree repo maybeFrom files = withRepo repo $ \repop ->
    maybeWith repop $ \fromp -> alloca $ \builderpp -> do
        check "treebuilder_create" $ G.c'git_treebuilder_create builderpp
            fromp
        builderp <- peek builderpp
        mapM_ (modify builderp) files
        oid <- alloca $ \oidp -> do
            check "treebuilder_write" $
                G.c'git_treebuilder_write oidp repop builderp
            peekOid oidp
        G.c'git_treebuilder_free builderp
        pure $ Tree oid
    where
    maybeWith repop io = case maybeFrom of
        Just tree -> withTree repop tree io
        Nothing -> io nullPtr
    modify builderp (name, val) = withCString name $ \namep -> case val of
        Nothing -> treebuilderRemove builderp name namep
        Just (Left blob) -> treebuilderInsertFile builderp name namep blob
        Just (Right tree) -> treebuilderInsertDir builderp name namep tree

-- | Apply a list of modifications to an existing tree.
modifyTree :: Repo -> Tree -> [Modification] -> IO Tree
modifyTree repo tree mods = withRepo repo $ \repop ->
    go repop (Just tree) (modificationsToDir mods)
    where
    go repop maybeTree entries =
        withMaybeTree repop maybeTree $ \treep -> alloca $ \builderpp -> do
            check "treebuilder_create" $
                G.c'git_treebuilder_create builderpp treep
            builderp <- peek builderpp
            mapM_ (modify repop builderp) entries
            oid <- alloca $ \oidp -> do
                check "treebuilder_write" $
                    G.c'git_treebuilder_write oidp repop builderp
                peekOid oidp
            G.c'git_treebuilder_free builderp
            pure $ Tree oid
    withMaybeTree _ Nothing io = io nullPtr
    withMaybeTree repop (Just tree) io = withTree repop tree io

    modify repop builderp (name, ModifyFile maybeBytes) =
        withCString name $ \namep -> case maybeBytes of
            Nothing -> treebuilderRemove builderp name namep
            Just bytes -> do
                blob <- writeBlobRepo repop bytes
                treebuilderInsertFile builderp name namep blob
    modify repop builderp (name, ModifyDir entries) =
        withCString name $ \namep -> do
            entryp <- G.c'git_treebuilder_get builderp namep
            maybeTree <- if entryp == nullPtr
                then pure Nothing
                else Just . Tree <$> (peekOid =<< G.c'git_tree_entry_id entryp)
            tree <- go repop maybeTree entries
            -- Delete empty directories automatically.  This shouldn't be
            -- necessary, but without it git_diff_tree_to_tree gets extraneous
            -- Removes.
            ifM (emptyTreeRepo repop tree)
                (treebuilderRemove builderp name namep)
                (treebuilderInsertDir builderp name namep tree)

treebuilderRemove :: Ptr G.C'git_treebuilder -> String -> CString -> IO ()
treebuilderRemove builderp name namep =
    check ("git_treebuilder_remove: " ++ show name) $
        G.c'git_treebuilder_remove builderp namep

treebuilderInsertFile :: Ptr G.C'git_treebuilder -> String -> CString
    -> Blob -> IO ()
treebuilderInsertFile builderp name namep (Blob oid) =
    check ("git_treebuilder_insert: " ++ show name) $ withOid oid $ \oidp ->
        G.c'git_treebuilder_insert nullPtr builderp namep oidp 0o100644

treebuilderInsertDir :: Ptr G.C'git_treebuilder -> String -> CString
    -> Tree -> IO ()
treebuilderInsertDir builderp name namep (Tree oid) =
    check ("git_treebuilder_insert: " ++ show (name ++ "/")) $
    withOid oid $ \oidp ->
        G.c'git_treebuilder_insert nullPtr builderp namep oidp 0o040000

readTree :: Repo -> Tree -> IO [(FileName, Either Blob Tree)]
readTree repo tree = withRepo repo $ \repop ->
    withTree repop tree $ \treep -> do
        count <- G.c'git_tree_entrycount treep
        entries <- mapM (G.c'git_tree_entry_byindex treep)
            (Lists.range' 0 count 1)
        mapM peekEntry entries
    where
    peekEntry entryp = do
        typ <- G.c'git_tree_entry_type entryp
        name <- peekCString =<< G.c'git_tree_entry_name entryp
        oid <- peekOid =<< G.c'git_tree_entry_id entryp
        val <- if typ == G.c'GIT_OBJ_TREE then pure (Right (Tree oid)) else
            if typ == G.c'GIT_OBJ_BLOB then pure (Left (Blob oid)) else
            throw $ show oid ++ " expected tree or blob: " ++ show typ
        pure (name, val)

emptyTreeRepo :: RepoP -> Tree -> IO Bool
emptyTreeRepo repop tree = withTree repop tree $ \treep -> do
    count <- G.c'git_tree_entrycount treep
    pure $ count == 0

withTree :: RepoP -> Tree -> (Ptr G.C'git_tree -> IO a) -> IO a
withTree repop (Tree oid) io = withOid oid $ \oidp -> alloca $ \treepp -> do
    check ("tree_lookup: " ++ show oid) $
        G.c'git_tree_lookup treepp repop oidp
    treep <- peek treepp
    io treep `Exception.finally` G.c'git_tree_free treep

-- * commit

parseCommit :: String -> Maybe Commit
parseCommit str
    | length str == 40 = Just $ Commit $ Char8.pack str
    | otherwise = Nothing

writeCommit :: Repo -> Text -> Text -> [Commit] -> Tree -> String
    -> IO Commit
writeCommit repo user email parents tree description =
    withRepo repo $ \repop -> withSig $ \sigp ->
    withCommits repop parents $ \parentsLen parentsp ->
    withTree repop tree $ \treep -> withCString description $ \descp ->
    withCString "HEAD" $ \headp -> alloca $ \commitp -> do
        check "write_commit" $ G.c'git_commit_create commitp repop headp
            sigp sigp nullPtr descp treep (fromIntegral parentsLen) parentsp
        toCommit <$> peekOid commitp
    where
    withSig io = withText user $ \userp -> withText email $ \emailp ->
        alloca $ \sigpp -> do
            check "signature_now" $ G.c'git_signature_now sigpp userp emailp
            sigp <- peek sigpp
            io sigp `Exception.finally` G.c'git_signature_free sigp
    withCommits repop commits io = go [] commits $ \ps -> withArrayLen ps io
        where
        go ps [] cont = cont (reverse ps)
        go ps (c:cs) cont = withCommit repop c $ \p -> go (p:ps) cs cont

data CommitData = CommitData
    { commitTree :: !Tree
    , commitParents :: ![Commit]
    , commitAuthor :: !String
    , commitText :: !String
    } deriving (Eq, Show)

readCommit :: Repo -> Commit -> IO CommitData
readCommit repo commit =
    withRepo repo $ \repop -> readCommitRepo repop commit

readCommitRepo :: RepoP -> Commit -> IO CommitData
readCommitRepo repop commit = withCommit repop commit $ \commitp -> do
    author <- peekUser =<< peek =<< G.c'git_commit_author commitp
    tree <- peekOid =<< G.c'git_commit_tree_id commitp
    parentsLen <- G.c'git_commit_parentcount commitp
    parents <- mapM peekOid
        =<< mapM (G.c'git_commit_parent_id commitp)
            (if parentsLen == 0 then [] else [0..parentsLen-1])
    desc <- peekCString =<< G.c'git_commit_message commitp
    pure $ CommitData (Tree tree) (map toCommit parents) author desc
    where
    peekUser sig = do
        name <- peekCString (G.c'git_signature'name sig)
        email <- peekCString (G.c'git_signature'email sig)
        pure $ name ++ " <" ++ email ++ ">"

withCommit :: RepoP -> Commit -> (Ptr G.C'git_commit -> IO a) -> IO a
withCommit repop commit io = withOid (fromCommit commit) $
    \oidp -> alloca $ \commitpp -> do
        check ("tree_lookup: " ++ show commit) $
            G.c'git_commit_lookup commitpp repop oidp
        commitp <- peek commitpp
        io commitp `Exception.finally` G.c'git_commit_free commitp

-- * diff

diffCommits :: Repo -> Commit -> Commit -> IO [Modification]
diffCommits repo old new = withRepo repo $ \repop -> do
    oldc <- readCommitRepo repop old
    newc <- readCommitRepo repop new
    diffTrees repo (commitTree oldc) (commitTree newc)

-- | Recursively diff two trees.
diffTrees :: Repo -> Tree -> Tree -> IO [Modification]
diffTrees repo old new =
    withRepo repo $ \repop -> diffTreeRepo repop old new

diffTreeRepo :: RepoP -> Tree -> Tree -> IO [Modification]
diffTreeRepo repop old new =
    withTree repop old $ \oldp -> withTree repop new $ \newp -> do
    diffs <- alloca $ \listpp -> do
        check "diff_tree" $
            G.c'git_diff_tree_to_tree listpp repop oldp newp nullPtr
        listp <- peek listpp
        ref <- IORef.newIORef []
        -- with_fptr (G.mk'git_diff_line_cb (diff_cb ref)) $ \callback ->
        --     check "diff_print" $ G.c'git_diff_print listp
        --         G.c'GIT_DIFF_FORMAT_NAME_STATUS callback nullPtr
        withFptr (G.mk'git_diff_data_cb (diffCb ref)) $ \callback ->
            check "diff_print_compact" $
                G.c'git_diff_print_compact listp callback nullPtr
        IORef.readIORef ref
    concatMapM toMod diffs
    where
    -- I'm only interested in which files were deleted, added, or modified.
    toMod (status, path, oid)
        | status == G.c'GIT_DELTA_DELETED = pure [Remove path]
        | status == G.c'GIT_DELTA_ADDED || status == G.c'GIT_DELTA_MODIFIED = do
            bytes <- readBlobRepo repop (Blob oid)
            pure [Add path bytes]
        | otherwise = throw $ "diff_trees " ++ show (old, new)
            ++ ": unknown status: " ++ show status
    diffCb ref deltap _rangep _lineOrigin _contentp _contentLen _data = do
        delta <- peek deltap
        let newFile = G.c'git_diff_delta'new_file delta
            status = G.c'git_diff_delta'status delta
        let oidBytes = G.c'git_oid'id (G.c'git_diff_file'oid newFile)
            pathp = G.c'git_diff_file'path newFile
        path <- peekCString pathp
        oid <- cucharToOid oidBytes
        IORef.modifyIORef ref ((status, path, oid):)
        pure 0

    -- Used for libgit2 0.20
    -- diff_cb ref deltap _hunkp _linep _payloadp = do
    --     G.C'git_diff_delta status _flags _similarity _nfiles
    --         _old_file new_file <- peek deltap
    --     let G.C'git_diff_file oid pathp _size _flags _mode = new_file
    --     path <- peekCString pathp
    --     IORef.modifyIORef ref ((status, path, oid):)
    --     return 0

-- * refs

writeRef :: Repo -> Commit -> Ref -> IO ()
writeRef repo commit ref = withRepo repo $ \repop ->
    withOid (fromCommit commit) $ \commitp -> withRefName ref $ \namep ->
    alloca $ \refpp -> do
        check ("write_ref " ++ show ref) $
            G.c'git_reference_create refpp repop namep commitp 1
        refp <- peek refpp
        when (refp /= nullPtr) $
            G.c'git_reference_free refp

readRef :: Repo -> Ref -> IO (Maybe Commit)
readRef repo ref = withRepo repo $ \repop -> readRefRepo repop ref

readRefRepo :: RepoP -> Ref -> IO (Maybe Commit)
readRefRepo repop ref = withRefName ref $ \namep ->
    alloca $ \oidp -> do
        code <- G.c'git_reference_name_to_id oidp repop namep
        if code /= G.c'GIT_OK then pure Nothing else do
        oid <- peekOid oidp
        pure (Just (toCommit oid))

withRef :: RepoP -> Ref -> (Ptr G.C'git_reference -> IO a) -> IO a
withRef repop ref io =
    withCString ref $ \namep -> alloca $ \refpp -> do
        check ("reference_lookup " ++ show ref) $
            G.c'git_reference_lookup refpp repop namep
        refp <- peek refpp
        io refp `Exception.finally` when (refp /= nullPtr)
            (G.c'git_reference_free refp)

-- | Read all the refs in the repo.
readRefs :: RepoP -> IO [Ref]
readRefs repop = alloca $ \arrayp -> do
    check "read_refs" $ G.c'git_reference_list arrayp repop
        G.c'GIT_REF_LISTALL
    G.C'git_strarray stringsp count <- peek arrayp
    strps <- peekArray (fromIntegral count) stringsp
    refs <- mapM (peekRefName "") strps
    G.c'git_strarray_free arrayp
    pure refs

-- | Read all refs along with their commits.
readRefMap :: Repo -> IO (Map Ref Commit)
readRefMap repo = withRepo repo $ \repop -> do
    refs <- readRefs repop
    commits <- mapM (readRefRepo repop) refs
    pure $ Map.fromList
        [(ref, commit) | (ref, Just commit) <- zip refs commits]

withRefName :: Ref -> (CString -> IO a) -> IO a
withRefName ref io = withCString ("refs" </> ref) $ \namep -> io namep

peekRefName :: String -> CString -> IO Ref
peekRefName prefix str = do
    (name, stripped) <- Lists.dropPrefix "refs/" <$> peekCString str
    unless stripped $ throw $ prefix ++ "wasn't in refs/: " ++ show name
    pure name

-- ** symbolic refs

writeSymbolicRef :: Repo -> Ref -> Ref -> IO ()
writeSymbolicRef repo sym ref
    | not (validSymbolicRef sym) = throw $ "ref should be ALL_CAPS: " ++ sym
    | otherwise = withRepo repo $ \repop ->
        withRefName ref $ \namep -> withCString sym $ \symp ->
        alloca $ \refpp -> do
            refp <- checkLookup "reference_symbolic_create" refpp $
                G.c'git_reference_symbolic_create refpp repop symp namep 1
            when (refp /= nullPtr) $
                G.c'git_reference_free refp

readSymbolicRef :: Repo -> Ref -> IO (Maybe Ref)
readSymbolicRef repo sym = withRepo repo $ \repop ->
    withRef repop sym $ \symp -> alloca $ \refpp -> do
        refp <- checkLookup "reference_resolve" refpp $
            G.c'git_reference_resolve refpp symp
        if refp == nullPtr then pure Nothing
            else fmap Just $ peekRefName ("ref of " ++ show sym ++ " ")
                =<< G.c'git_reference_name refp

-- | Rationale documented at: https://github.com/libgit2/libgit2/pull/938
validSymbolicRef :: Ref -> Bool
validSymbolicRef ref =
    not (null ref) && all (\c -> Char.isAsciiUpper c || c == '_') ref
        && head ref /= '_' && last ref /= '_'

-- ** HEAD ref

-- | Point HEAD to a commit.
updateHead :: Repo -> Commit -> IO ()
updateHead repo commit = do
    maybeRef <- readSymbolicRef repo "HEAD"
    maybe (throw "HEAD symbolic ref missing") (writeRef repo commit) maybeRef

readHeadCommit :: Repo -> IO (Maybe Commit)
readHeadCommit repo = readRef repo =<< readHead repo

writeHead :: Repo -> Ref -> IO ()
writeHead repo = writeSymbolicRef repo "HEAD"

readHead :: Repo -> IO Ref
readHead repo = maybe (throw "HEAD symbolic ref missing") pure =<<
    readSymbolicRef repo "HEAD"

-- * revwalk

-- | Get commits in reverse chronological order from the given ref.
readLog :: Repo -> Ref -> IO [Commit]
readLog repo ref = withRepo repo $ \repop -> withRefName ref $ \refnamep ->
    withRevwalk repop $ \walkp -> do
        check ("revwalk_push_ref: " ++ ref) $
            G.c'git_revwalk_push_ref walkp refnamep
        walk walkp [SortTime]

-- | Read commits starting from the given commit.
readLogFrom :: Repo -> Commit -> IO [Commit]
readLogFrom repo commit = withRepo repo $ \repop ->
    withOid (fromCommit commit) $ \oidp -> withRevwalk repop $ \walkp -> do
        check ("revwalk_push: " ++ show commit) $
            G.c'git_revwalk_push walkp oidp
        walk walkp [SortTime]

-- | Get commits in reverse chronological order from the HEAD.
readLogHead :: Repo -> IO [Commit]
readLogHead repo = readLog repo =<< readHead repo

data SortFlag = SortTopological | SortTime | SortReverse deriving (Show)

walk :: Ptr G.C'git_revwalk -> [SortFlag] -> IO [Commit]
walk walkp flags = do
    G.c'git_revwalk_sorting walkp
        (foldl' (Bits..|.) gitSortNone (map flag flags))
    alloca $ \oidp -> whileJust (next oidp)
    where
    next oidp = do
        errno <- G.c'git_revwalk_next oidp walkp
        if errno == G.c'GIT_ITEROVER then pure Nothing else do
        check "revwalk_next" (pure errno)
        oid <- peekOid oidp
        pure (Just (toCommit oid))
    whileJust io = do
        maybeVal <- io
        case maybeVal of
            Nothing -> pure []
            Just val -> do
                vals <- whileJust io
                pure (val : vals)
    flag f = case f of
        SortTopological -> gitSortTopological
        SortTime -> gitSortTime
        SortReverse -> gitSortReverse

-- | These aren't declared as a type at the C level, so the binding doesn't
-- have them.
gitSortNone, gitSortTopological, gitSortTime, gitSortReverse :: CUInt
gitSortNone = 0
gitSortTopological = 1
gitSortTime = 2
gitSortReverse = 4

withRevwalk :: RepoP -> (Ptr G.C'git_revwalk -> IO a) -> IO a
withRevwalk repop io = alloca $ \walkpp -> do
    check "revwalk_new" $ G.c'git_revwalk_new walkpp repop
    walkp <- peek walkpp
    io walkp `Exception.finally` G.c'git_revwalk_free walkp

-- * dir

type Dir = Map FileName File
data File = File ByteString | Dir Dir deriving (Eq, Show)

makeDir :: [(FilePath, ByteString)] -> Either String Dir
makeDir = foldM merge Map.empty
    where
    -- System.FilePath is incorrect because git always uses /s.
    merge dir (path, bytes) = insert dir (Lists.split (=='/') path) bytes
    insert _ [] bytes = Left $ "can't insert into empty path: " ++ show bytes
    insert files [name] bytes = pure $ Map.insert name (File bytes) files
    insert files (name : names) bytes = do
        subs <- case Map.lookup name files of
            Just (Dir subs) -> pure subs
            Just (File _) ->
                Left $ "can't insert below a file: " ++ show name
            Nothing -> pure Map.empty
        subs <- insert subs names bytes
        pure $ Map.insert name (Dir subs) files

flattenDir :: Dir -> [(FilePath, ByteString)]
flattenDir = concatMap flatten . Map.toList
    where
    flatten (name, File bytes) = [(name, bytes)]
    flatten (name, Dir dir) = map (first (name </>)) (flattenDir dir)

writeDir :: Repo -> Dir -> IO Tree
writeDir repo filemap = do
    let files = Map.toList filemap
    hashes <- mapM (write . snd) files
    writeTree repo Nothing (zip (map fst files) (map Just hashes))
    where
    write (File bytes) = Left <$> writeBlob repo bytes
    write (Dir dir) = Right <$> writeDir repo dir

readDir :: Repo -> Tree -> IO Dir
readDir repo tree = do
    (names, files) <- unzip <$> readTree repo tree
    files <- mapM read files
    pure $ Map.fromList $ zip names files
    where
    read (Left blob) = File <$> readBlob repo blob
    read (Right tree) = Dir <$> readDir repo tree

-- * misc

-- | GC the repo.  As far as I know, there's no way to do this with libgit2, so
-- it invokes the standalone git command.
gc :: Repo -> IO ()
gc repo = git repo ["gc", "--aggressive"] "" >> pure ()

git :: Repo -> [String] -> ByteString -> IO ByteString
git repo = gitEnv repo []

gitEnv :: Repo -> [(String, String)] -> [String] -> ByteString
    -> IO ByteString
gitEnv repo env args stdin = do
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
        _ -> pure out

-- * error

type Error = CInt

errorMsg :: Error -> Maybe String
errorMsg errno
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

newtype GitException = GitException String
instance Exception.Exception GitException
instance Show GitException where
    show (GitException msg) = "GitException: " ++ msg

throw :: String -> IO a
throw = Exception.throwIO . GitException

check :: String -> IO Error -> IO ()
check caller action = do
    errno <- action
    case errorMsg errno of
        Nothing -> pure ()
        Just msg -> throw $ caller ++ ": " ++ msg

checkLookup :: String -> Ptr (Ptr a) -> IO Error -> IO (Ptr a)
checkLookup caller ptrptr io = do
    errno <- io
    if errno == G.c'GIT_ENOTFOUND then pure nullPtr else do
    case errorMsg errno of
        Nothing -> peek ptrptr
        Just msg -> throw $ caller ++ ": " ++ msg

-- * util

withFptr :: IO (FunPtr a) -> (FunPtr a -> IO b) -> IO b
withFptr make io = do
    fptr <- make
    io fptr `Exception.finally` freeHaskellFunPtr fptr

withText :: Text -> (CString -> IO a) -> IO a
withText = ByteString.useAsCString . Encoding.encodeUtf8
