-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Git.Git2_test where
import qualified Data.Map as Map
import qualified System.Directory as Directory

import Util.Control
import qualified Util.File as File
import qualified Util.Git.Git2 as Git
import Util.Git.Git2 (Modification(..))
import Util.Test


test_misc = do
    repo <- new_repo
    io_equal (Git.init repo) True -- created by 'new_repo'
    blob <- Git.write_blob repo "abc"
    io_equal (Git.read_blob repo blob) "abc"

    tree <- Git.write_tree repo Nothing [("filename", Just (Left blob))]
    io_equal (Git.read_tree repo tree) [("filename", Left blob)]
    tree2 <- Git.write_tree repo Nothing
        [("dirname", Just (Right tree)), ("file2", Just (Left blob))]
    io_equal (Git.read_tree repo tree2)
        [("dirname", Right tree), ("file2", Left blob)]

    commit <- Git.write_commit repo "me" "email" [] tree "commit by me\n"
    commit2 <- Git.write_commit repo "me" "email" [commit] tree
        "another commit by me\n"
    io_equal (Git.read_commit repo commit) $
        Git.CommitData tree [] "me <email>" "commit by me\n"

    -- refs
    -- Updated automatically by write_commit.
    io_equal (Git.read_ref repo "heads/master") (Just commit2)
    io_equal (Git.read_head_commit repo) (Just commit2)

    let ref = "tags/0"
    Git.write_ref repo commit ref
    io_equal (Git.read_ref repo ref) (Just commit)
    io_equal (Git.read_ref repo "no-such-ref") Nothing
    io_equal (Git.read_ref_map repo) $
        Map.fromList [("heads/master", commit2), ("tags/0", commit)]

    -- sym -> ref
    Git.write_symbolic_ref repo "SYM" ref
    io_equal (Git.read_symbolic_ref repo "SYM") (Just ref)

    -- HEAD -> ref -> commit
    Git.write_head repo ref
    io_equal (Git.read_head_commit repo) (Just commit)
    -- HEAD -> ref -> commit2
    Git.update_head repo commit2
    io_equal (Git.read_head_commit repo) (Just commit2)
    io_equal (Git.read_head repo) ref

    -- revlist
    io_equal (Git.read_log repo ref) [commit2, commit]
    io_equal (Git.read_log_head repo) [commit2, commit]

test_make_dir = do
    let f = Git.make_dir
    equal (f [("foo/bar", "abc"), ("foo/bar/baz", "def")])
        (Left "can't insert below a file: \"bar\"")
    equal (f [("a/b", "a"), ("a/c", "b")]) $ Right $
        Map.fromList [("a", Git.Dir (Map.fromList
            [("b", Git.File "a"), ("c", Git.File "b")]))]

test_write_dir = do
    repo <- new_repo
    let dir1 = expect_right "make_dir" $
            Git.make_dir [("a/b", "abc"), ("d", "def")]
    tree <- Git.write_dir repo dir1
    dir2 <- Git.read_dir repo tree
    equal dir1 dir2

test_modify_tree = do
    repo <- new_repo
    let read tree = Git.flatten_dir <$> Git.read_dir repo tree
    tree <- Git.write_dir repo Map.empty
    tree <- Git.modify_tree repo tree [Add "a/b" "abc", Add "c" "def"]
    io_equal (read tree) [("a/b", "abc"), ("c", "def")]
    tree <- Git.modify_tree repo tree [Remove "a/b", Add "c" "qqq"]
    io_equal (read tree) [("c", "qqq")]

test_diff_trees = do
    repo <- new_repo
    let dir = expect_right "make_dir" $
            Git.make_dir [("a/b", "abc"), ("d", "def")]
    tree1 <- Git.write_dir repo dir
    tree2 <- Git.modify_tree repo tree1 [Add "a/b" "def"]
    io_equal (Git.diff_trees repo tree1 tree2) [Add "a/b" "def"]

    tree3 <- Git.modify_tree repo tree1 [Remove "a/b"]
    io_equal (Git.diff_trees repo tree2 tree3) [Remove "a/b"]
    pprint =<< Git.read_dir repo tree2
    pprint =<< Git.read_dir repo tree3

test_modifications_to_dir = do
    let f = Git.modifications_to_dir
    equal (f [Add "a/b/c" "abc", Add "a/b/c" "def"])
        [("a", Git.ModifyDir [("b", Git.ModifyDir
            [("c", Git.ModifyFile (Just "def"))])])]
    equal (f [Remove "x/y", Remove "x"])
        [("x", Git.ModifyDir [("y", Git.ModifyFile Nothing)]),
            ("x", Git.ModifyFile Nothing)]

-- * implementation

new_repo = do
    let repo = "build/test/test.git"
    File.ignoreEnoent $ Directory.removeDirectoryRecursive repo
    io_equal (Git.init repo) False
    return repo
