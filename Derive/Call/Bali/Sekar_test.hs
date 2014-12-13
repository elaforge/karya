-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Bali.Sekar_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest


test_sekar = do
    let extract = DeriveTest.extract DeriveTest.e_note
        run sekar_event notes = extract $ DeriveTest.derive_tracks_linear
            "import bali.sekar | sekar-arrive=f | infer-duration 4" $
            (">", [sekar_event]) : UiTest.note_track notes

    let make_cd d1 d2 = [(0, d1, "4c"), (d1, d2, "4d")]
        cd = make_cd 1 1

    equal (run (0, 4, "sekar 'a b'") [])
        ([], ["Error: pattern chars must be a-z: \"a b\""])
    equal (run (0, 4, "sekar ab") cd)
        ([(0, 2, "4c"), (2, 2, "4d")], [])
    equal (run (0, 4, "sekar abab") cd)
        ([(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c"), (3, 1, "4d")], [])
    equal (run (0, 4, "sekar abAb") cd)
        ([(0, 1, "4c"), (1, 1, "4d"), (3, 1, "4d")], [])

    -- If there are moe notes than pattern, repeat the pattern.
    -- Gaps become rests.
    equal (run (0, 8, "sekar abab") (cd ++ [(3, 1, "4e")]))
        ([ (0, 1, "4c"), (1, 1, "4d"), (2, 1, "4c"), (3, 1, "4d")
         , (5, 1, "4e"), (7, 1, "4e")
         ], [])

    -- arrive=t
    equal (run (0, 4, "sekar-arrive=t | sekar abab") cd)
        ([(1, 1, "4c"), (2, 1, "4d"), (3, 1, "4c"), (4, 4, "4d")], [])
    -- Differing lengths.
    equal (run (0, 6, "sekar-arrive=t | sekar abab") (make_cd 2 1))
        ([(1, 2, "4c"), (3, 1, "4d"), (4, 2, "4c"), (6, 4, "4d")], [])
    equal (run (0, 6, "sekar-arrive=t | sekar abab") (make_cd 1 2))
        ([(2, 1, "4c"), (3, 2, "4d"), (5, 1, "4c"), (6, 4, "4d")], [])

