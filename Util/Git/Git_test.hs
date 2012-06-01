{-# LANGUAGE OverloadedStrings #-}
module Util.Git.Git_test where
import qualified Data.Map as Map
import qualified System.Directory as Directory

import qualified Util.File as File
import qualified Util.Git.Git as Git
import Util.Test


test_misc = do
    repo <- new_repo
    io_equal (Git.init repo) True -- created by 'new_repo'
    blob <- Git.write_blob repo "abc"
    io_equal (Git.read_blob repo blob) "abc"
    tree <- Git.write_tree repo [("filename", Left blob)]
    io_equal (Git.read_tree repo tree) [("filename", Left blob)]
    commit <- Git.write_commit repo "me" "email" [] tree "commit by me\n"
    commit_data <- Git.read_commit repo commit
    -- The author line has a date in it that will vary.
    equal (commit_data { Git.commit_author = "" }) $
        Git.CommitData tree [] "" "commit by me\n"
    let ref = "heads/master"
    Git.write_ref repo commit ref
    io_equal (Git.read_ref repo ref) (Just commit)
    io_equal (Git.read_ref repo "no-such-ref") Nothing
    Git.write_head repo ref
    io_equal (Git.read_head repo) ref
    Git.update_head repo commit
    io_equal (Git.read_head_commit repo) (Just commit)

test_make_dir = do
    let f = Git.make_dir
    equal (f [("foo/bar", "abc"), ("foo/bar/baz", "def")])
        (Left "can't insert below a file: \"bar\"")
    equal (f [("a/b", "a"), ("a/c", "b")]) $ Right $
        Map.fromList [("a", Git.Dir (Map.fromList
            [("b", Git.File "a"), ("c", Git.File "b")]))]

test_write_dir = do
    repo <- new_repo
    let Right dir1 = Git.make_dir [("a/b", "abc"), ("d", "def")]
    tree <- Git.write_dir repo dir1
    print tree
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

new_repo = do
    let repo = "build/test/test.git"
    File.ignore_enoent $ Directory.removeDirectoryRecursive repo
    io_equal (Git.init repo) False
    return repo
