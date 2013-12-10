-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Gamakam_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN
import Types


test_kampita = do
    let run call end = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks
                [(">", [(0, 4, "")]), ("*", [(0, 0, call), (end, 0, "3c")])]
    equal (run "kam (4c) 1 1 1" 3)
        ([[(0, NN.c4), (1, NN.cs4), (2, NN.c4), (3, NN.c3)]], [])
    equal (run "kam (4c) 1d 1 1" 3)
        ([[(0, NN.c4), (1, NN.d4), (2, NN.c4), (3, NN.c3)]], [])

    equal (run "h | kam (4c) 1 1 1" 3)
        ([[(0, NN.c4), (2, NN.cs4), (3, NN.c3)]], [])

test_kampita_c = do
    let run = run_diatonic (map snd . DeriveTest.e_nns)
        run2 = run_diatonic DeriveTest.e_nns
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
    equal (run2 "kam 1 .5 2" 4)
        ([[(0, 60), (1, 61), (2, 62), (3, 61), (4, 60)]], [])
    -- Lilt.
    equal (run2 "kam-lilt = .5 | kam 1 1 1" 4)
        ([[(0, 60), (1.5, 62), (2, 60), (3.5, 62), (4, 60)]], [])
    equal (run2 "kam-lilt = -.5 | kam 1 1 1" 4)
        ([[(0, 60), (0.5, 62), (2, 60), (2.5, 62), (4, 60)]], [])

test_dip = do
    let run ex call end = DeriveTest.extract ex $ DeriveTest.derive_tracks
            [(">", [(0, 4, "")]), ("*", [(0, 0, call), (end, 0, "3c")])]
    equal (run DeriveTest.e_nns "dip (4c) 1 -1 1" 4)
        ([[(0, NN.d4), (1, NN.b3), (2, NN.d4), (3, NN.b3), (4, NN.c3)]], [])
    equal (run DeriveTest.e_dyn "dip (4c) 1 -1 1 .5" 4)
        ([[(0, 1), (1, 0.5), (2, 1), (3, 0.5), (4, 1)]], [])

run_diatonic :: (Score.Event -> a) -> String -> ScoreTime -> ([a], [String])
run_diatonic extract call end =
    DeriveTest.extract extract $ DeriveTest.derive_tracks
        [ (">", [(0, end, "")])
        , ("*", [(0, 0, "4c")])
        , ("t-diatonic", [(0, 0, call), (end, 0, "--")])
        ]

test_jaru = do
    let run call = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks
                [(">", [(0, 4, "")]), ("*", [(0, 0, call)])]
    equal (run "sgr (3c) 2")
        ([[(0, 47), (1, 48), (2, 50), (3, 49), (4, 48)]], [])
