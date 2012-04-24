{-# LANGUAGE NoMonomorphismRestriction #-}
module Util.Seq_test where
import Util.Test
import qualified Util.Seq as Seq


test_keyed_group_on = do
    let f = Seq.keyed_group_on fst
    equal (f ([] :: [(Int, Char)])) []
    equal (f [(5, 'a'), (0, 'b'), (5, 'c')])
        [(0, [(0, 'b')]), (5, [(5, 'a'), (5, 'c')])]

test_merge_lists = do
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
        [(Just 'a', Just 'a'), (Just 'b', Just 'b'), (Just 'c', Just 'c')]
    equal (f "abc" "axbc")
        [(Just 'a', Just 'a'), (Nothing, Just 'x'), (Just 'b', Just 'b'),
            (Just 'c', Just 'c')]
    equal (f "abc" "axxbc")
        [(Just 'a', Just 'a'), (Nothing, Just 'x'), (Nothing, Just 'x'),
            (Just 'b', Just 'b'), (Just 'c', Just 'c')]
    equal (f "abc" "bc")
        [(Just 'a', Nothing), (Just 'b', Just 'b'), (Just 'c', Just 'c')]
    equal (f "abc" "xyz")
        [(Just 'a', Nothing), (Just 'b', Nothing), (Just 'c', Nothing),
            (Nothing, Just 'x'), (Nothing, Just 'y'), (Nothing, Just 'z')]
