{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}
-- | Low level interaction with the git object store.
--
-- This uses the git cmdline, which is easy but inefficient for lots of
-- small operations.
module Util.Git.Git where
import Prelude hiding (init)
import qualified Control.Exception as Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable

import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Process as Process
import qualified Util.Seq as Seq


newtype Blob = Blob Hash deriving (Eq, Ord, Show)
newtype Tree = Tree Hash deriving (Eq, Ord, Show)
newtype Commit = Commit Hash deriving (Eq, Ord, Show)
type Hash = ByteString

instance Pretty.Pretty Blob where pretty (Blob hash) = unparse_hash hash
instance Pretty.Pretty Tree where pretty (Tree hash) = unparse_hash hash
instance Pretty.Pretty Commit where pretty (Commit hash) = unparse_hash hash

type Repo = FilePath
-- | Repo-internal path.
type FileName = FilePath
-- | This has the initial refs/ stripped off.
type Ref = FilePath

-- | True if it already existed.
init :: Repo -> IO Bool
init repo = ifM is_git (return True) $ do
    -- Prevent it from copying misc crap from /usr/share/git-core/templates.
    git repo ["init", "--template="] ""
    return False
    where
    is_git = andM [Directory.doesDirectoryExist repo,
        Directory.doesDirectoryExist (repo </> "objects")]

write_blob :: Repo -> ByteString -> IO Blob
write_blob repo bytes = Blob . parse_hash <$>
    git repo ["hash-object", "-w", "--stdin"] bytes

read_blob :: Repo -> Blob -> IO ByteString
read_blob repo (Blob hash) =
    git repo ["cat-file", "blob", unparse_hash hash] ""

write_tree :: Repo -> [(FileName, Either Blob Tree)] -> IO Tree
write_tree repo files =
    Tree . parse_hash <$> git repo ["mktree"] (Char8.unlines (map mkline files))
    where
    -- mode SP type SP sha1 TAB path
    mkline (name, Left (Blob hash)) =
        "100644 blob " <> hash <> "\t" <> UTF8.fromString name
    mkline (name, Right (Tree hash)) =
        "040000 tree " <> hash <> "\t" <> UTF8.fromString name

read_tree :: Repo -> Tree -> IO [(FileName, Either Blob Tree)]
read_tree repo (Tree tree) = do
    out <- git repo ["ls-tree", unparse_hash tree] ""
    mapM parse (Char8.lines out)
    where
    parse line = case Char8.words line of
        [_, typ, hash, name]
            | typ == "blob" -> return
                (Char8.unpack name, Left (Blob (parse_hash hash)))
            | typ == "tree" -> return
                (Char8.unpack name, Right (Tree (parse_hash hash)))
            | otherwise ->
                throw $ "unknown type from ls-tree: " ++ show typ
        words -> throw $ "unparseable line from ls-tree: " ++ show words

parse_commit :: String -> Maybe Commit
parse_commit str
    | length str == 40 = Just $ Commit $ Char8.pack str
    | otherwise = Nothing

write_commit :: Repo -> String -> String -> [Commit] -> Tree -> String
    -> IO Commit
write_commit repo user email parents (Tree tree) description =
    Commit . parse_hash <$> git_env repo (env user email)
        ("commit-tree" : unparse_hash tree : ps)
        (UTF8.fromString description)
    where
    -- git will crash if it can't figure out the name and email.
    env name email =
        [ ("GIT_AUTHOR_NAME", name)
        , ("GIT_AUTHOR_EMAIL", email)
        , ("GIT_COMMITTER_NAME", name)
        , ("GIT_COMMITTER_EMAIL", email)
        ]
    ps = concat [["-p", hash_of p] | p <- parents]
    hash_of (Commit hash) = unparse_hash hash

data CommitData = CommitData {
    commit_tree :: !Tree
    , commit_parents :: ![Commit]
    , commit_author :: !String
    , commit_text :: !String
    } deriving (Eq, Show)

read_commit :: Repo -> Commit -> IO CommitData
read_commit repo (Commit commit) =
    parse =<< git repo ["cat-file", "-p", unparse_hash commit] ""
    where
    -- Output looks like 'tree hexhex\nparent hexhex\n...
    parse bytes = do
        tree <- require "tree"
        let parents = map Commit $ Map.get [] "parent" header
        author <- require "author"
        return $ CommitData (Tree tree) parents (UTF8.toString author)
            (UTF8.toString (Char8.unlines (drop 1 desc_lines)))
        where
        (header_lines, desc_lines) = break Char8.null (Char8.lines bytes)
        header = Map.multimap (map split header_lines)
        require field = case Map.lookup field header of
            Just (val : _) -> return val
            _ -> throw $ "read_commit: missing field "
                ++ show field ++ " from " ++ show bytes
    split s = (pre, Char8.drop 1 post)
        where (pre, post) = Char8.break (==' ') s

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

read_log_from :: Repo -> Commit -> IO [Commit]
read_log_from repo (Commit commit) = map (Commit . parse_hash) . Char8.lines <$>
    git repo ["rev-list", unparse_hash commit] ""

-- | Get commits in reverse chronological order from the HEAD.
read_log_head :: Repo -> IO [Commit]
read_log_head repo =
    -- Be careful to return [] on an empty repo (which has a broken HEAD ref).
    maybe (return []) (read_log_from repo) =<< read_head_commit repo

gc :: Repo -> IO ()
gc repo = void $ git repo ["gc", "--aggressive"] ""

-- ** refs

write_ref :: Repo -> Commit -> Ref -> IO ()
write_ref repo (Commit commit) ref =
    void $ git repo ["update-ref", "refs" </> ref, unparse_hash commit] ""

read_ref :: Repo -> Ref -> IO (Maybe Commit)
read_ref repo ref =
    (Just . Commit . parse_hash <$>
        git repo ["show-ref", "--verify", "--hash", "refs" </> ref] "")
        `Exception.catch` (\(_exc :: GitException) -> return Nothing)

read_ref_map :: Repo -> IO (Map.Map Ref Commit)
read_ref_map repo = do
    -- For some reason, show-ref returns 1 if there are no tags.
    (_, out, _) <- run_git repo [] ["show-ref", "--tags"] ""
    Map.fromList <$> mapM parse (Char8.lines out)
    where
    parse line = case Char8.split ' ' line of
        [hash, ref] -> return (Char8.unpack (strip ref), Commit hash)
        _ -> throw $ "show-ref: unparseable line: " ++ show line
    strip = Char8.takeWhile (not . Char.isSpace) . Char8.drop 1
        . Char8.dropWhile (/='/')

write_symbolic_ref :: Repo -> FileName -> Ref -> IO ()
write_symbolic_ref repo name ref =
    void $ git repo ["symbolic-ref", name, "refs" </> ref] ""

read_symbolic_ref :: Repo -> FileName -> IO (Maybe Ref)
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

type Dir = Map.Map FileName File
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

read_dir :: Repo -> Tree -> IO Dir
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

modify_tree :: Repo -> Tree -> [Modification] -> IO Tree
modify_tree repo (Tree tree) mods = do
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

-- * implementation

newtype GitException = GitException String deriving (Typeable.Typeable)
instance Exception.Exception GitException
instance Show GitException where
    show (GitException msg) = "GitException: " ++ msg

parse_hash :: ByteString -> Hash
parse_hash = fst . Char8.spanEnd (=='\n')

unparse_hash :: Hash -> String
unparse_hash = Char8.unpack

throw :: String -> IO a
throw = Exception.throwIO . GitException

git :: Repo -> [String] -> ByteString -> IO ByteString
git repo = git_env repo []

git_env :: Repo -> [(String, String)] -> [String] -> ByteString -> IO ByteString
git_env repo env args stdin = do
    (code, out, err) <- run_git repo env args stdin
    -- let sin = " <" ++ show (Char8.length stdin)
    -- let sin = if Char8.null stdin then "" else " <" ++ show stdin
    -- putStrLn $ unwords ("git" : args) ++ sin ++ " ==> " ++ Seq.strip (Char8.unpack out)
    if code == 0 then return out
        else throw $ unwords ("git" : args) ++ " returned " ++ show code
            ++ " " ++ Seq.strip (Char8.unpack err)

run_git :: Repo -> [(String, String)] -> [String] -> ByteString
    -> IO (Int, ByteString, ByteString)
run_git repo env args stdin = do
    (code, out, err) <- Process.readProcessWithExitCode
        (Just (("GIT_DIR", repo) : env)) "git" args stdin
    return $ case code of
        Exit.ExitFailure c -> (c, out, err)
        Exit.ExitSuccess -> (0, out, err)
