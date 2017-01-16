-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Deriver.Lib_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


-- Lots of stuff that should probably be in here is in Derive.Derive_test.

test_with_scale = do
    let run title = DeriveTest.extract Score.initial_nn
            . DeriveTest.derive_tracks title . UiTest.note_track1
    -- Setting the scale replaces the calls.
    equal (run "scale=twelve | scale=twelve-r" ["4c", "4s"])
        ([Nothing, Just 60], ["Error: pitch generator not found: 4c"])
