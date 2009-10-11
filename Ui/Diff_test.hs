module Ui.Diff_test where
import Util.Test

import qualified Ui.Diff as Diff


-- The main 'diff' part is indirectly tested by Sync_test.

test_pair_lists = do
    let f = Diff.pair_lists (==)
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
