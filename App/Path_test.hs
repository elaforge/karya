-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module App.Path_test where
import qualified App.Path as Path

import           Util.Test


test_in_dir :: Test
test_in_dir = do
    let f = Path.in_dir
    equal (f "a/b" "a") True
    equal (f "ab" "a") False
    equal (f "a" "a") False
    equal (f "a/b" "a/") True
    equal (f "ab" "a/") False
    equal (f "a/b" "a/") True
    equal (f "ab" "a/") False
    equal (f "a" "a/") False
