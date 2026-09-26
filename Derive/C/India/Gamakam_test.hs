-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.India.Gamakam_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import Global
import Types


test_kampita :: Test
test_kampita = do
    let run call end = DeriveTest.extract DeriveTest.eNnsOld $ deriveTracks
            [ (">", [(0, 4, "")])
            , ("*", [(0, 0, call), (end, 0, "3c")])
            ]
    equal (run "kam (4c) 1 1 1" 3)
        ([[(0, NN.c4), (1, NN.cs4), (2, NN.c4), (3, NN.c3)]], [])
    equal (run "kam (4c) 1d 1 1" 3)
        ([[(0, NN.c4), (1, NN.d4), (2, NN.c4), (3, NN.c3)]], [])
    equal (run "h 1 | kam (4c) 1 1 1" 3)
        ([[(0, NN.c4), (2, NN.cs4), (3, NN.c3)]], [])

test_kampita_c :: Test
test_kampita_c = do
    let run = runDiatonic (map snd . DeriveTest.eNnsOld)
        run2 = runDiatonic DeriveTest.eNnsOld
    -- Trill goes to the last time, but won't create a short cycle.
    equal (run2 "kam 1 1 1" 2) ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run2 "kam 1 1 1" 2.5) ([[(0, 60), (1, 62), (2, 60)]], [])

    equal (run2 "h 1 | kam 1 1 1" 2) ([[(0, 60), (2, 62)]], [])
    equal (run "^kam 1 1 1" 2) ([[62, 60, 62]], [])
    equal (run "_kam 1 1 1" 2) ([[60, 62, 60]], [])

    equal (run "kam^ 1 1 1" 2) ([[60, 62]], [])
    equal (run "kam_ 1 1 1" 2) ([[60, 62, 60]], [])
    equal (run "kam^ -1 1 1" 2) ([[60, 59, 60]], [])
    equal (run "kam_ -1 1 1" 2) ([[60, 59]], [])

    -- Transition time.
    equal (run2 "kam 1 .5 2" 4) ([[(0, 60), (2, 62), (4, 60)]], [])
    -- Lilt.
    equal (run2 "kam-lilt = .5 | kam 1 1 1" 4)
        ([[(0, 60), (1.5, 62), (2, 60), (3.5, 62), (4, 60)]], [])
    equal (run2 "kam-lilt = -.5 | kam 1 1 1" 4)
        ([[(0, 60), (0.5, 62), (2, 60), (2.5, 62), (4, 60)]], [])

    -- Adjust.
    equal (run2 "adjust = shorten | kam^ 1 1 1" 2) ([[(0, 60), (1, 62)]], [])
    equal (run2 "adjust = stretch | kam^ 1 1 1" 2) ([[(0, 60), (2, 62)]], [])

test_nkampita_c :: Test
test_nkampita_c = do
    let run = runDiatonic DeriveTest.eNnsOld
    stringsLike (snd (run "nkam 1 0" 2)) ["cycles: expected Signal (>0)"]
    equal (run "nkam 1 1" 3) ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run "nkam 1 1" 6) ([[(0, 60), (2, 62), (4, 60)]], [])
    equal (run "transition = 2 | nkam 1 1" 9)
        ([[(0, 60), (3, 62), (6, 60)]], [])
    equal (run "nkam 1 2" 5)
        ([[(0, 60), (1, 62), (2, 60), (3, 62), (4, 60)]], [])
    equal (run "nkam_ 1 2" 5)
        ([[(0, 60), (1, 62), (2, 60), (3, 62), (4, 60)]], [])
    equal (run "nkam^ 1 2" 4) ([[(0, 60), (1, 62), (2, 60), (3, 62)]], [])

test_dip :: Test
test_dip = do
    let run ex call end = DeriveTest.extract ex $ deriveTracks
            [(">", [(0, 4, "")]), ("*", [(0, 0, call), (end, 0, "3c")])]
    equal (run DeriveTest.eNnsOld "dip (4c) 1 -1 1" 4)
        ([[(0, NN.d4), (1, NN.b3), (2, NN.d4), (3, NN.b3), (4, NN.c3)]], [])
    equal (run DeriveTest.eDynRounded "dip (4c) 1 -1 1 .5" 4)
        ( [ [ (0, 1), (0.96, 1), (1, 0.5), (1.96, 0.5), (2, 1)
            , (2.96, 1), (3, 0.5), (4, 0.5), (4, 1)
            ]
          ]
        , []
        )

test_jaru :: Test
test_jaru = do
    let run call = DeriveTest.extract DeriveTest.eNns $
            deriveTracks [(">", [(0, 4, "")]), ("*", [(0, 0, call)])]
    equal (run "sgr (3c) 2") ([[(0, 47), (2, 50), (4, 48)]], [])

runDiatonic :: (Score.Event -> a) -> Text -> ScoreTime -> ([a], [Text])
runDiatonic extract call end =
    DeriveTest.extract extract $ deriveTracks
        [ (">", [(0, end, "")])
        , ("*", [(0, 0, "4c")])
        , ("t-dia", [(0, 0, call), (end, 0, "--|")])
        ]

deriveTracks :: [UiTest.TrackSpec] -> Derive.Result
deriveTracks = DeriveTest.deriveTracks "import india.gamakam"
