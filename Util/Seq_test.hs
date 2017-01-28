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

test_equal_pairs = do
    let f = Seq.equal_pairs (==)
    equal (f "abc" "abc")
        [Both 'a' 'a', Both 'b' 'b', Both 'c' 'c']
    equal (f "abc" "axbc")
        [Both 'a' 'a', Second 'x', Both 'b' 'b', Both 'c' 'c']
    equal (f "abc" "axxbc")
        [Both 'a' 'a', Second 'x', Second 'x', Both 'b' 'b', Both 'c' 'c']
    equal (f "abc" "bc")
        [First 'a', Both 'b' 'b', Both 'c' 'c']
    equal (f "abc" "xyz")
        [First 'a', First 'b', First 'c', Second 'x', Second 'y', Second 'z']
