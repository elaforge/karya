-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Gamakam_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.NN as NN


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
    let run call end = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks
                [ (">", [(0, 4, "")])
                , ("*", [(0, 0, "4c")])
                , ("t-diatonic", [(0, 0, call), (end, 0, "--")])
                ]
    equal (run "kam 1 1 1" 3) ([[(0, NN.c4), (1, NN.d4), (2, NN.c4)]], [])
    equal (run "h 1 | kam 1 1 1" 3) ([[(0, NN.c4), (2, NN.d4)]], [])

test_dip = do
    let run ex call end = DeriveTest.extract ex $ DeriveTest.derive_tracks
            [(">", [(0, 4, "")]), ("*", [(0, 0, call), (end, 0, "3c")])]
    equal (run DeriveTest.e_nns "dip (4c) 1 -1 1" 4)
        ([[(0, NN.d4), (1, NN.b3), (2, NN.d4), (3, NN.b3), (4, NN.c3)]], [])
    equal (run DeriveTest.e_dyn "dip (4c) 1 -1 1 .5" 4)
        ([[(0, 1), (1, 0.5), (2, 1), (3, 0.5), (4, 1)]], [])

test_jaru = do
    let run call = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks
                [(">", [(0, 4, "")]), ("*", [(0, 0, call)])]
    equal (run "sgr (3c) 2")
        ([[(0, 47), (1, 48), (2, 50), (3, 49), (4, 48)]], [])
