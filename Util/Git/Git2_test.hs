{-# LANGUAGE OverloadedStrings #-}
module Util.Git.Git2_test where
import qualified Data.Map as Map
import qualified System.Directory as Directory

import qualified Util.File as File
import qualified Util.Git.Git2 as Git
import Util.Test


test_misc = do
    repo <- new_repo
    io_equal (Git.init repo) True -- created by 'new_repo'
    blob <- Git.write_blob repo "abc"
    io_equal (Git.read_blob repo blob) "abc"

    tree <- Git.write_tree repo [("filename", Left blob)]
    io_equal (Git.read_tree repo tree) [("filename", Left blob)]
    tree2 <- Git.write_tree repo [("dirname", Right tree), ("file2", Left blob)]
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
    Git.write_symbolic_ref repo "sym" ref
    io_equal (Git.read_symbolic_ref repo "sym") (Just ref)

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

test_modify_dir = do
    repo <- new_repo
    tree <- Git.write_dir repo Map.empty
    tree <- Git.modify_dir repo tree [Git.Add "a/b" "abc", Git.Add "c" "def"]
    io_equal (Git.read_dir repo tree) $ Map.fromList
        [ ("a", Git.Dir (Map.fromList [("b", Git.File "abc")]))
        , ("c", Git.File "def")
        ]
    tree <- Git.modify_dir repo tree [Git.Remove "a/b", Git.Add "c" "qqq"]
    io_equal (Git.read_dir repo tree) $ Map.fromList [("c", Git.File "qqq")]

test_diff_trees = do
    repo <- new_repo
    let dir = expect_right "make_dir" $
            Git.make_dir [("a/b", "abc"), ("d", "def")]
    tree1 <- Git.write_dir repo dir
    tree2 <- Git.modify_dir repo tree1 [Git.Add "a/b" "def"]
    io_equal (Git.diff_trees repo tree1 tree2) [Git.Add "a/b" "def"]
    tree3 <- Git.modify_dir repo tree1 [Git.Remove "a/b"]
    io_equal (Git.diff_trees repo tree2 tree3) [Git.Remove "a/b"]


-- * implementation

new_repo = do
    let repo = "build/test/test.git"
    File.ignore_enoent $ Directory.removeDirectoryRecursive repo
    io_equal (Git.init repo) False
    return repo
