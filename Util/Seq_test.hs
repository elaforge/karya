-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NoMonomorphismRestriction #-}
module Util.Seq_test where
import Util.Test
import qualified Util.Seq as Seq
import Util.Seq (Paired(..))


test_range_end = do
    let f = Seq.range_end
    equal (f 0 1 0.4) [0, 0.4, 0.8, 1]
    equal (f 0 1 0.5) [0, 0.5, 1]
    equal (f 1 0 (-0.5)) [1, 0.5, 0]
    equal (f 1 0 0.5) [0]
    equal (f 0 1 (-0.5)) [1]

test_keyed_group_sort = do
    let f = Seq.keyed_group_sort fst
    equal (f ([] :: [(Int, Char)])) []
    equal (f [(5, 'a'), (0, 'b'), (5, 'c')])
        [(0, [(0, 'b')]), (5, [(5, 'a'), (5, 'c')])]
    -- Each group comes back in its original order.
    equal (f [(5, 'a'), (5, 'c'), (5, 'b')])
        [(5, [(5, 'a'), (5, 'c'), (5, 'b')])]

test_merge_asc_lists = do
    let f = Seq.merge_asc_lists fst

    equal (f [[(0, "0a"), (1, "1a")], [(1, "1b"), (2, "2b")]])
        [(0, "0a"), (1, "1a"), (1, "1b"), (2, "2b")]
    equal (f [[(0, "0a"), (1, "1a")], [(0, "0b"), (2, "2b")]])
        [(0, "0a"), (0, "0b"), (1, "1a"), (2, "2b")]

    let infinite = [[(j, show j ++ "." ++ show i) | j <- [i..i+4]] | i <- [0..]]
    -- sufficiently lazy
    equal (take 30 (f infinite))
        (take 30 (Seq.merge_lists fst (take 30 infinite)))

test_diff_index = do
    let f = Seq.diff_index (==)
    equal (f "ab" "ab") [(0, Both 'a' 'a'), (1, Both 'b' 'b')]
    -- 'b' is missing from "ac" at index 1.
    equal (f "abc" "ac")
        [(0, Both 'a' 'a'), (1, First 'b'), (1, Both 'c' 'c')]
    -- 'b' is in "abc" at index 1.
    equal (f "ac" "abc")
        [(0, Both 'a' 'a'), (1, Second 'b'), (2, Both 'c' 'c')]

test_drop_dups = do
    let f = Seq.drop_dups id
    equal (f "abc") "abc"
    equal (f "abbc") "abc"
    equal (f "abbbc") "abc"
    equal (f "abbbcccca") "abca"

test_split_before = do
    let f = Seq.split_before (==1)
    equal (f []) []
    equal (f [1, 2, 3, 1, 2]) [[], [1, 2, 3], [1, 2]]
    equal (f [2, 3, 1, 2]) [[2, 3], [1, 2]]

test_split_after = do
    let f = Seq.split_after (==1)
    equal (f []) []
    equal (f [1, 2, 3, 1, 2]) [[1], [2, 3, 1], [2]]
    equal (f [2, 3, 1, 2]) [[2, 3, 1], [2]]

test_drop_before = do
    let f n = Seq.head $ Seq.drop_before id n [1..4]
    equal (f 0) (Just 1)
    equal (f 1) (Just 1)
    equal (f 1.5) (Just 1)
    equal (f 2) (Just 2)
