-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Git_test where
import qualified Data.Map as Map

import qualified Util.Git as Git
import Util.Git (Modification(..))
import Util.Test
import qualified Util.Test.Testing as Testing


test_misc :: Test
test_misc = Git.initialize $ do
    repo <- newRepo
    ioEqual (Git.init repo) True -- created by 'new_repo'
    blob <- Git.writeBlob repo "abc"
    ioEqual (Git.readBlob repo blob) "abc"

    tree <- Git.writeTree repo Nothing [("filename", Just (Left blob))]
    ioEqual (Git.readTree repo tree) [("filename", Left blob)]
    tree2 <- Git.writeTree repo Nothing
        [("dirname", Just (Right tree)), ("file2", Just (Left blob))]
    ioEqual (Git.readTree repo tree2)
        [("dirname", Right tree), ("file2", Left blob)]

    commit <- Git.writeCommit repo "me" "email" [] tree "commit by me\n"
    commit2 <- Git.writeCommit repo "me" "email" [commit] tree
        "another commit by me\n"
    ioEqual (Git.readCommit repo commit) $
        Git.CommitData tree [] "me <email>" "commit by me\n"

    -- refs
    -- Updated automatically by write_commit.
    ioEqual (Git.readRef repo "heads/master") (Just commit2)
    ioEqual (Git.readHeadCommit repo) (Just commit2)

    let ref = "tags/0"
    Git.writeRef repo commit ref
    ioEqual (Git.readRef repo ref) (Just commit)
    ioEqual (Git.readRef repo "no-such-ref") Nothing
    ioEqual (Git.readRefMap repo) $
        Map.fromList [("heads/master", commit2), ("tags/0", commit)]

    -- sym -> ref
    Git.writeSymbolicRef repo "SYM" ref
    ioEqual (Git.readSymbolicRef repo "SYM") (Just ref)

    -- HEAD -> ref -> commit
    Git.writeHead repo ref
    ioEqual (Git.readHeadCommit repo) (Just commit)
    -- HEAD -> ref -> commit2
    Git.updateHead repo commit2
    ioEqual (Git.readHeadCommit repo) (Just commit2)
    ioEqual (Git.readHead repo) ref

    -- revlist
    ioEqual (Git.readLog repo ref) [commit2, commit]
    ioEqual (Git.readLogHead repo) [commit2, commit]

test_make_dir :: Test
test_make_dir = Git.initialize $ do
    let f = Git.makeDir
    equal (f [("foo/bar", "abc"), ("foo/bar/baz", "def")])
        (Left "can't insert below a file: \"bar\"")
    equal (f [("a/b", "a"), ("a/c", "b")]) $ Right $
        Map.fromList [("a", Git.Dir (Map.fromList
            [("b", Git.File "a"), ("c", Git.File "b")]))]

test_write_dir :: Test
test_write_dir = Git.initialize $ do
    repo <- newRepo
    let dir1 = expectRight $ Git.makeDir [("a/b", "abc"), ("d", "def")]
    tree <- Git.writeDir repo dir1
    dir2 <- Git.readDir repo tree
    equal dir1 dir2

test_modify_tree :: Test
test_modify_tree = Git.initialize $ do
    repo <- newRepo
    let read tree = Git.flattenDir <$> Git.readDir repo tree
    tree <- Git.writeDir repo Map.empty
    tree <- Git.modifyTree repo tree [Add "a/b" "abc", Add "c" "def"]
    ioEqual (read tree) [("a/b", "abc"), ("c", "def")]
    tree <- Git.modifyTree repo tree [Remove "a/b", Add "c" "qqq"]
    ioEqual (read tree) [("c", "qqq")]

test_diff_trees :: Test
test_diff_trees = Git.initialize $ do
    repo <- newRepo
    let makeDir = expectRight . Git.makeDir
    tree1 <- Git.writeDir repo (makeDir [("a/b", "abc"), ("d", "def")])
    tree2 <- Git.modifyTree repo tree1 [Add "a/b" "def"]
    ioEqual (Git.diffTrees repo tree1 tree2) [Add "a/b" "def"]

    tree3 <- Git.modifyTree repo tree1 [Remove "a/b"]
    ioEqual (Git.diffTrees repo tree2 tree3) [Remove "a/b"]
    ioEqual (Git.readDir repo tree2) (makeDir [("a/b", "def"), ("d", "def")])
    ioEqual (Git.readDir repo tree3) (makeDir [("d", "def")])

test_modifications_to_dir :: Test
test_modifications_to_dir = Git.initialize $ do
    let f = Git.modificationsToDir
    equal (f [Add "a/b/c" "abc", Add "a/b/c" "def"])
        [("a", Git.ModifyDir [("b", Git.ModifyDir
            [("c", Git.ModifyFile (Just "def"))])])]
    equal (f [Remove "x/y", Remove "x"])
        [("x", Git.ModifyDir [("y", Git.ModifyFile Nothing)]),
            ("x", Git.ModifyFile Nothing)]

-- * implementation

newRepo :: IO FilePath
newRepo = do
    repo <- Testing.tmpDir "git"
    ioEqual (Git.init repo) False
    pure repo
