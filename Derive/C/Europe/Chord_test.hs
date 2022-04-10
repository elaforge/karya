-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Europe.Chord_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_stack :: Test
test_stack = do
    let run call = DeriveTest.extract extract $
            DeriveTest.derive_tracks "import europe"
            [(">", [(0, 2, call)]), ("*", [(0, 0, "4c")])]
        extract e = (Score.event_start e, DeriveTest.e_pitch e)
    equal (run "stack 3 3") ([(0, "4c"), (0, "4e"), (0, "4g")], [])
    equal (run "stack m3 M3") ([(0, "4c"), (0, "4d#"), (0, "4g")], [])
    equal (run "stack-time = 1 | stack^ p5") ([(0, "4c"), (1, "4g")], [])
    equal (run "stack-time = 1 | stack_ p5") ([(0, "4g"), (1, "4c")], [])
    -- Compresses if there isn't enough time.
    equal (run "stack-time = 2 | stack^ p5") ([(0, "4c"), (1, "4g")], [])
