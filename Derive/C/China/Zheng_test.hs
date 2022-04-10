-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.China.Zheng_test where
import qualified Util.Num as Num
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN
import qualified Ui.UiTest as UiTest

import           Util.Test


test_gliss :: Test
test_gliss = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks
                "import china.zheng | open-strings=(list (3a) (4c) (4e))"
            . UiTest.note_track
        extract e =
            ( Score.event_start e
            , Num.roundDigits 2 <$> Score.initial_nn e
            )
    equal (run [(0, 1, "gliss -1 1s -- 4c")])
        ([(-1, Just NN.a3), (0, Just NN.c4)], [])
    equal (run [(1, 1, "gliss 1 1s -- 4c")])
        ([(0, Just NN.e4), (1, Just NN.c4)], [])
    equal (run [(0, 1, "gliss -1 1s -- tr (4c)")])
        ([(-1, Just NN.a3), (0, Just NN.c4)], [])
