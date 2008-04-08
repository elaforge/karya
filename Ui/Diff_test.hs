module Ui.Diff_test where
import Util.Test
import Ui.Diff

test_edit_distance = do
    let dist = edit_distance (==)
    equal (dist "abc" "abc") [Same, Same, Same]
    equal (dist "abc" "axbc") [Same, Insert 'x', Same, Same]
    equal (dist "abc" "axxbc") [Same, Insert 'x', Insert 'x', Same, Same]
    equal (dist "abc" "bc") [Delete, Same, Same]
    equal (dist "abc" "xyz")
        [Delete, Delete, Delete, Insert 'x', Insert 'y', Insert 'z']
