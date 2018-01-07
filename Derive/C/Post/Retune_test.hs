-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Post.Retune_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.NN as NN


test_retune = do
    let run = DeriveTest.extract DeriveTest.e_nns_rounded
            . DeriveTest.derive_tracks
                "import retune | realize-retune | retune-time=2 | retune-dist=4"
            . UiTest.note_track
    equal (run [(0, 4, "retune | -- 4c")]) ([[(0, NN.c4)]], [])
    equal (run [(0, 1, "4c"), (1, 4, "retune | -- 4c")])
        ([[(0, NN.c4)], [(1, NN.c4)]], [])
    equal (run [(0, 1, "4c"), (1, 4, "retune | -- 5c")])
        ([[(0, NN.c4)], [(1, 72.6), (2, 72.1), (3, 72)]], [])
