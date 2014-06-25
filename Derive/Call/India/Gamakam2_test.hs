-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Gamakam2_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest


test_sequence = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks "import india.gamakam2"
            . UiTest.note_track
        extract = DeriveTest.e_nns
    strings_like (snd (run [(0, 8, "! ; no-call ; -- 4c")]))
        ["generator not found"]

    -- Implicit hold.
    equal (run [(0, 8, "! -- 4c")]) ([[(0, 60), (8, 60)]], [])
    equal (run [(0, 8, "! ;; -- 4c")]) ([[(0, 60), (8, 60)]], [])

    -- From call.
    equal (run [(0, 1, "4c"), (1, 4, "! -^ 2 -- 4d")])
        ([[(0, 60)], [(1, 60), (2, 61), (3, 62), (5, 62)]], [])
    equal (run [(0, 4, "! p 1 1 -- 4c")])
        ([[(0, 61), (1, 60), (4, 60)]], [])

    -- To call.
    equal (run [(0, 4, "! ; - ; p 2 2 -- 4c")])
        ([[(0, 60), (2, 60), (3, 61), (4, 62)]], [])
    equal (run [(0, 4, "! ;; p 2 2 -- 4c")])
        ([[(0, 60), (2, 60), (3, 61), (4, 62)]], [])

    -- Enough room.
    equal (run [(0, 8, "! ;; p 2 2 -- 4c")])
        ([[(0, 60), (6, 60), (7, 61), (8, 62)]], [])
    equal (run [(0, 2, "! ;; p 2 2 -- 4c")])
        ([[(0, 60), (1, 61), (2, 62)]], [])
    -- Not enough room.
    equal (run [(0, 1, "! ;; p 2 2 -- 4c")])
        ([[(0, 60), (1, 62)]], [])

    -- Medium is divided evenly.
    equal (run [(0, 8, "! ; - 1 ; - 0 ; -- 4c")])
        ([[(0, 61), (4, 60), (8, 60)]], [])

    -- Begin, middle, end.
    equal (run [(0, 8, "! p 1 1 ;; p 1 1 -- 4c")])
        ([[(0, 61), (1, 60), (7, 60), (8, 61)]], [])
    -- Middle divided equally between 59 and 60.
    equal (run [(0, 8, "! p 1 1; - -1; - 0; p 1 1 -- 4c")])
        ([[(0, 61), (1, 59), (4, 60), (7, 60), (8, 61)]], [])
