-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Gamakam_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.NN as NN


test_wobble = do
    let run call end = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks
                [(">", [(0, 4, "")]), ("*", [(0, 0, call), (end, 0, "3c")])]
    equal (run "wobble (4c) 1 1 1" 3)
        ([[(0, NN.c4), (1, NN.cs4), (2, NN.c4), (3, NN.c3)]], [])

test_jaru = do
    let run call = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks
                [(">", [(0, 4, "")]), ("*", [(0, 0, call)])]
    equal (run "sgr (3c) 2")
        ([[(0, 47), (1, 48), (2, 50), (3, 49), (4, 48)]], [])
