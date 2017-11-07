-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.China.Zheng_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN
import Global


title :: Text
title = "import china.zheng | open-strings = (list (4c) (4d) (4e) (4g) (4a))"

test_gliss = do
    let run gliss dest = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks title
            [ ("> | scale=twelve-r | key=g-maj", [(4, 1, gliss)])
            , ("*", [(4, 0, dest)])
            ]
    -- Pitches are correct for relative scale.
    equal (run "gliss -1 1" "4s") ([Just NN.e4, Just NN.g4], [])

test_gliss_a = do
    let run gliss dest = DeriveTest.extract extract $
            DeriveTest.derive_tracks title
            [(">", [(4, 1, gliss)]), ("*", [(4, 0, dest)])]
        extract e = (DeriveTest.e_note e, Score.initial_dynamic e)
    equal (run "gliss-a 2 1 .5" "4f")
        ([((3, 0.5, "4a"), 0.5), ((3.5, 0.5, "4g"), 0.75), ((4, 1, "4f"), 1)],
            [])
    equal (run "gliss-a -2 1" "4f")
        ([((3, 0.5, "4d"), 1), ((3.5, 0.5, "4e"), 1), ((4, 1, "4f"), 1)], [])
    equal (run "gliss-a 2 1" "4d")
        ([((3, 0.5, "4g"), 1), ((3.5, 0.5, "4e"), 1), ((4, 1, "4d"), 1)], [])

    let run2 gliss dest = DeriveTest.extract Score.event_start $
            DeriveTest.derive_tracks title
            [ ("tempo", [(0, 0, "1"), (2, 0, "2")]), (">", [(4, 1, gliss)])
            , ("*", [(4, 0, dest)])
            ]
    -- 0   1   2   3   4
    -- 0   1   2   2.5 3
    equal (run2 "gliss-a 2 1t" "4f") ([2.5, 2.75, 3], [])
    equal (run2 "gliss-a 2 1s" "4f") ([2, 2.5, 3], [])
