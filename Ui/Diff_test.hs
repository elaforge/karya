module Ui.Diff_test where
import Util.Test
import Ui.Diff

test_pair_lists = do
    let pair = pair_lists (==)
    equal (pair "abc" "abc")
        [(Just 'a', Just 'a'), (Just 'b', Just 'b'), (Just 'c', Just 'c')]
    equal (pair "abc" "axbc")
        [(Just 'a', Just 'a'), (Nothing, Just 'x'), (Just 'b', Just 'b'),
            (Just 'c', Just 'c')]
    equal (pair "abc" "axxbc")
        [(Just 'a', Just 'a'), (Nothing, Just 'x'), (Nothing, Just 'x'),
            (Just 'b', Just 'b'), (Just 'c', Just 'c')]
    equal (pair "abc" "bc")
        [(Just 'a', Nothing), (Just 'b', Just 'b'), (Just 'c', Just 'c')]
    equal (pair "abc" "xyz")
        [(Just 'a', Nothing), (Just 'b', Nothing), (Just 'c', Nothing),
            (Nothing, Just 'x'), (Nothing, Just 'y'), (Nothing, Just 'z')]
