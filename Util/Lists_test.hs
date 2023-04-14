-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Lists_test where
import qualified Util.Lists as Lists

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
    equal (f [1, 2, 3, 1, 2]) [[], [1, 2, 3], [1, 2]]
    equal (f [2, 3, 1, 2]) [[2, 3], [1, 2]]
