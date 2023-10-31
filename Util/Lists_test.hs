-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Lists_test where
import qualified Util.Lists as Lists
import           Util.Lists (Paired(..))

import           Util.Test


test_splitWith :: Test
test_splitWith = do
    let f = Lists.splitWith match
        match = \case
            'c' -> Just 'c'
            'e' -> Just 'e'
            _ -> Nothing
    equal (f "") ("", [])
    equal (f "c") ("", [('c', "")])
    equal (f "abcd") ("ab", [('c', "d")])
    equal (f "abcdef") ("ab", [('c', "d"), ('e', "f")])

test_splitBefore :: Test
test_splitBefore = do
    let f = Lists.splitBefore (==1)
    equal (f []) []
    equal (f [1, 1]) [[], [1], [1]]
    equal (f [1, 2, 3, 1, 2]) [[], [1, 2, 3], [1, 2]]
    equal (f [2, 3, 1, 2]) [[2, 3], [1, 2]]

test_splitAfter :: Test
test_splitAfter = do
    let f = Lists.splitAfter (==1)
    equal (f []) []
    equal (f [1, 1]) [[1], [1]]
    equal (f [1, 2, 1]) [[1], [2, 1]]
    equal (f [1, 2, 3, 1, 2]) [[1], [ 2, 3, 1], [ 2]]
    equal (f [2, 3, 1, 2]) [[2, 3, 1], [2]]

test_rangeEnd :: Test
test_rangeEnd = do
    let f = Lists.rangeEnd
    equal (f 0 1 0.4) [0, 0.4, 0.8, 1]
    equal (f 0 1 0.5) [0, 0.5, 1]
    equal (f 1 0 (-0.5)) [1, 0.5, 0]
    equal (f 1 0 0.5) [0]
    equal (f 0 1 (-0.5)) [1]

test_keyedGroupSort :: Test
test_keyedGroupSort = do
    let f = Lists.keyedGroupSort fst
    equal (f ([] :: [(Int, Char)])) []
    equal (f [(5, 'a'), (0, 'b'), (5, 'c')])
        [(0, [(0, 'b')]), (5, [(5, 'a'), (5, 'c')])]
    -- Each group comes back in its original order.
    equal (f [(5, 'a'), (5, 'c'), (5, 'b')])
        [(5, [(5, 'a'), (5, 'c'), (5, 'b')])]

test_mergeAscLists :: Test
test_mergeAscLists = do
    let f = Lists.mergeAscLists fst

    equal (f [[(0, "0a"), (1, "1a")], [(1, "1b"), (2, "2b")]])
        [(0, "0a"), (1, "1a"), (1, "1b"), (2, "2b")]
    equal (f [[(0, "0a"), (1, "1a")], [(0, "0b"), (2, "2b")]])
        [(0, "0a"), (0, "0b"), (1, "1a"), (2, "2b")]

    let infinite = [[(j, show j ++ "." ++ show i) | j <- [i..i+4]] | i <- [0..]]
    -- sufficiently lazy
    equal (take 30 (f infinite))
        (take 30 (Lists.mergeLists fst (take 30 infinite)))

test_diffIndex :: Test
test_diffIndex = do
    let f = Lists.diffIndex (==)
    equal (f "ab" "ab") [(0, Both 'a' 'a'), (1, Both 'b' 'b')]
    -- 'b' is missing from "ac" at index 1.
    equal (f "abc" "ac")
        [(0, Both 'a' 'a'), (1, First 'b'), (1, Both 'c' 'c')]
    -- 'b' is in "abc" at index 1.
    equal (f "ac" "abc")
        [(0, Both 'a' 'a'), (1, Second 'b'), (2, Both 'c' 'c')]

test_dropDups :: Test
test_dropDups = do
    let f = Lists.dropDups id
    equal (f "abc") "abc"
    equal (f "abbc") "abc"
    equal (f "abbbc") "abc"
    equal (f "abbbcccca") "abca"

test_dropBefore :: Test
test_dropBefore = do
    let f n = Lists.head $ Lists.dropBefore id n [1..4]
    equal (f 0) (Just 1)
    equal (f 1) (Just 1)
    equal (f 1.5) (Just 1)
    equal (f 2) (Just 2)
