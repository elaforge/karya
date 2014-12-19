-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Bali.Sekar_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import Global


test_sekar = do
    let extract = DeriveTest.extract DeriveTest.e_note
        run sekar_event notes = extract $ DeriveTest.derive_tracks_linear
            "import bali.sekar | sekar-arrive=f" $
            (">", [sekar_event]) : UiTest.note_track notes
    let cd = [(2, 2, "4c"), (4, 2, "4d")]

    equal (run (2, 4, "sekar 'a b'") [])
        ([], ["Error: pattern chars must be a-z: \"a b\""])
    -- 0   2   4   6   8   10
    --     |------>
    --     c---d-->
    --     c-d-c-d>
    equal (run (2, 4, "sekar ab") cd)
        ([(2, 2, "4c"), (4, 2, "4d")], [])
    equal (run (2, 4, "sekar abab") cd)
        ([(2, 1, "4c"), (3, 1, "4d"), (4, 1, "4c"), (5, 1, "4d")], [])
    equal (run (2, 4, "sekar abAb") cd)
        ([(2, 1, "4c"), (3, 1, "4d"), (5, 1, "4d")], [])

    -- Trailing rest.
    equal (run (2, 4, "sekar abbA") cd)
        ([(2, 1, "4c"), (3, 1, "4d"), (4, 1, "4d")], [])
    -- 0   2   4   6   8   10
    --     |------->
    --     c-->
    --     c>  c>
    equal (run (2, 4, "sekar abab") [(2, 2, "4c")])
        ([(2, 1, "4c"), (4, 1, "4c")], [])
    equal (run (2, 4, "sekar abab") [(4, 2, "4c")])
        ([(3, 1, "4c"), (5, 1, "4c")], [])

    -- If there are moe notes than pattern, repeat the pattern.
    -- Gaps become rests.
    -- 0   2   4   6   8   10
    --     |-------------->
    --     c---d---e-->
    --     c-d-c-d-e>  e>
    equal (run (2, 8, "sekar abab") (cd ++ [(6, 2, "4e")]))
        ([ (2, 1, "4c"), (3, 1, "4d"), (4, 1, "4c"), (5, 1, "4d")
         , (6, 1, "4e"), (8, 1, "4e")
         ], [])

    -- 0   2   4   6   8   10
    --     |-------------->
    --     c---d-->    e-->
    --     c-d-c-d>  e>  e>
    equal (run (2, 8, "sekar abab") (cd ++ [(8, 2, "4e")]))
        ([ (2, 1, "4c"), (3, 1, "4d"), (4, 1, "4c"), (5, 1, "4d")
         , (7, 1, "4e"), (9, 1, "4e")
         ], [])

test_sekar_arrive = do
    let extract = DeriveTest.extract DeriveTest.e_note
        run sekar_event notes = extract $ DeriveTest.derive_tracks_linear
            "import bali.sekar | sekar-arrive=t | infer-duration 4" $
            (">", [sekar_event]) : UiTest.note_track notes
    let make durs = zip3 starts durs (map ("4"<>) ["c", "d", "e", "f", "g"])
            where starts = scanl (+) 2 durs

    equal (run (2, 4, "sekar ab") (make [2, 2, 2]))
        ([(4, 2, "4d"), (6, 2, "4e")], [])
    equal (run (2, 4, "sekar abab") (make [2, 2, 2]))
        ([(3, 1, "4d"), (4, 1, "4e"), (5, 1, "4d"), (6, 2, "4e")], [])
    equal (run (2, 8, "sekar abab") (make [2, 2, 2, 2, 2]))
        ([ (3, 1, "4d"), (4, 1, "4e"), (5, 1, "4d"), (6, 1, "4e")
         , (7, 1, "4f"), (8, 1, "4g"), (9, 1, "4f"), (10, 2, "4g")
         ], [])

    -- Differing lengths.
    -- 0   2   4   6   8   10
    --     |---------->
    --     c---d-------e-->
    --       d---e-d---e--- - -
    equal (run (2, 6, "sekar abab") (make [2, 4, 2]))
        ([(3, 2, "4d"), (5, 1, "4e"), (6, 2, "4d"), (8, 2, "4e")], [])

    -- 0   2   4   6   8   10  12
    --     |---------->
    --     c-------d---e-->
    --         d-e---d-e--- - -
    equal (run (2, 6, "sekar abab") (make [4, 2, 2]))
        ([(4, 1, "4d"), (5, 2, "4e"), (7, 1, "4d"), (8, 2, "4e")], [])

    -- 0   2   4   6   8   10  12
    --     |------>
    --     c---d---e---------->
    --       d-e-d-e--- - -
    equal (run (2, 4, "sekar abab") (make [2, 2, 6]))
        ([(3, 1, "4d"), (4, 1, "4e"), (5, 1, "4d"), (6, 6, "4e")], [])
    equal (run (2, 4, "sekar abab") (make [2, 2]))
        ([(4, 1, "4d"), (6, 4, "4d")], [])
