-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Bali.Sekar_test where
import qualified Derive.C.Bali.Sekar as Sekar
import           Derive.C.Bali.Sekar (DivNote(..))
import qualified Derive.Call.SubT as SubT
import qualified Derive.DeriveTest as DeriveTest

import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.UiTest as UiTest

import           Types
import           Util.Test


test_sekar = do
    let run sekar_event notes = extract $ DeriveTest.derive_tracks_linear
            "import bali.sekar | sekar-arrive=f" $
            (">", [sekar_event]) : UiTest.note_track notes
        extract = DeriveTest.extract DeriveTest.e_note
    let cd = [(2, 2, "4c"), (4, 2, "4d")]

    equal (run (2, 4, "sekar 'a b'") [])
        ([], ["pattern chars must be a-z: \"a b\""])
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

test_sekar_multiple = do
    let run dur notes = extract $ DeriveTest.derive_tracks_linear
            "import bali.sekar | sekar-arrive=f" $
            (">", [(0, dur, "sekar (list AaAa abab abac abcd)")])
                : UiTest.note_track notes
        extract = DeriveTest.extract DeriveTest.e_note
    equal (run 8 [(0, 4, "4c"), (4, 4, "4d")])
        ([(0, 2, "4c"), (2, 2, "4d"), (4, 2, "4c"), (6, 2, "4d")], [])
    equal (run 6 [(0, 3, "4c"), (3, 1.5, "4d"), (4.5, 1.5, "4e")])
        ([(0, 2, "4c"), (2, 1, "4d"), (3, 2, "4c"), (5, 1, "4e")], [])

test_sekar_arrive = do
    let run e = DeriveTest.extract DeriveTest.e_note . run_ e
        run_ sekar_event notes = DeriveTest.derive_tracks_linear
            "import bali.sekar | sekar-arrive=t | cancel 4" $
            (">", [sekar_event]) : UiTest.note_track notes

    equal (run (2, 4, "sekar ab") (make_notes [2, 2, 2]))
        ([(4, 2, "4a"), (6, 2, "4b")], [])
    equal (run (2, 4, "sekar abab") (make_notes [2, 2, 2]))
        ([(3, 1, "4a"), (4, 1, "4b"), (5, 1, "4a"), (6, 2, "4b")], [])
    equal (run (2, 8, "sekar abab") (make_notes [2, 2, 2, 2, 2]))
        ([ (3, 1, "4a"), (4, 1, "4b"), (5, 1, "4a"), (6, 1, "4b")
         , (7, 1, "4c"), (8, 1, "4d"), (9, 1, "4c"), (10, 2, "4d")
         ], [])

    -- Differing lengths.
    -- 0   2   4   6   8   10
    --     |---------->
    --     g---a-------b-->
    --       a---b-a---b--- - -
    equal (run (2, 6, "sekar abab") (make_notes [2, 4, 2]))
        ([(3, 2, "4a"), (5, 1, "4b"), (6, 2, "4a"), (8, 2, "4b")], [])

    -- 0   2   4   6   8   10  12
    --     |---------->
    --     c-------a---b-->
    --         a-b---a-b--- - -
    equal (run (2, 6, "sekar abab") (make_notes [4, 2, 2]))
        ([(4, 1, "4a"), (5, 2, "4b"), (7, 1, "4a"), (8, 2, "4b")], [])

    -- 0   2   4   6   8   10  12
    --     |------>
    --     c---a---b---------->
    --       a-b-a-b--- - -
    equal (run (2, 4, "sekar abab") (make_notes [2, 2, 6]))
        ([(3, 1, "4a"), (4, 1, "4b"), (5, 1, "4a"), (6, 6, "4b")], [])
    equal (run (2, 4, "sekar abab") (make_notes [2, 2]))
        ([(4, 1, "4a"), (6, 4, "4a")], [])

make_notes :: [ScoreTime] -> [UiTest.EventSpec]
make_notes durs = zip3 starts durs pitches
    where
    starts = scanl (+) 2 durs
    pitches = map ("4"<>) ["g", "a", "b", "c", "d", "e", "f"]

test_sekar_regular_arrive = do
    let run sekar_event notes =
            DeriveTest.extract DeriveTest.e_note $ run_ sekar_event notes
        run2 sekar_event notes =
            DeriveTest.extract DeriveTest.e_start_note $ run_ sekar_event notes
        run_ sekar_event notes = DeriveTest.derive_tracks_linear
            "import bali.sekar | sekar-arrive=t | cancel 4" $
            (">", [sekar_event]) : UiTest.note_track notes
    -- 0   2   4   6   8   10
    --     |------>
    --     g---a---b-->
    --       a-b-a-b--
    equal (run (2, 4, "sekar-e abab") (make_notes [2, 2, 2]))
        ([(3, 1, "4a"), (4, 1, "4b"), (5, 1, "4a"), (6, 2, "4b")], [])

    -- 012345678
    --   |---->
    --   g-a-b-c-
    --    a ab c
    equal (run2 (2, 6, "sekar-e aCabAc") (make_notes [2, 2, 2, 2]))
        ([(3, "4a"), (5, "4a"), (6, "4b"), (8, "4c")], [])

    -- 012345678
    --   |---->
    --   g-_-b-c-
    --    _c_b_c
    equal (run2 (2, 6, "sekar-e acabac")
            [(2, 2, "4g"), (6, 2, "4b"), (8, 2, "4c")])
        ([(4, "4c"), (6, "4b"), (8, "4c")], [])

    -- Difference between a rest and a continued note:

    -- 012345678
    --   |---->
    --   g-a---c-
    --    aca-ac
    equal (run (2, 6, "sekar-e acabac")
            [(2, 2, "4g"), (4, 4, "4a"), (8, 2, "4c")])
        ([(3, 1, "4a"), (4, 1, "4c"), (5, 2, "4a"), (7, 1, "4a"), (8, 2, "4c")],
            [])
    -- 012345678
    --   |---->
    --   g-a-  c-
    --    aca ac
    equal (run (2, 6, "sekar-e acabac")
            [(2, 2, "4g"), (4, 2, "4a"), (8, 2, "4c")])
        ([(3, 1, "4a"), (4, 1, "4c"), (5, 1, "4a"), (7, 1, "4a"), (8, 2, "4c")],
            [])

test_drop_until_next = do
    let f t = Sekar.drop_until_next (>t)
    equal (f 1 [0..4]) [1..4]
    equal (f 2.5 [0..4]) [2..4]
    equal (f 4 [0..4]) [4]
    equal (f 5 [0..4]) [4]

test_div_extract = do
    let f events = Sekar.div_extract (map mkevent events)
        mkevent (s, d, n) = SubT.EventT s d n
    -- 0   2   4   6   8   10  12
    -- a----b--_---c---d--
    --     x     x   x x       x
    -- Normally the sample times would be at regular intervals, but this
    -- way is convenient for testing.
    equal (f
            [ (0, 2, Just 'a'), (2 + ScoreTime.eta/2, 2, Just 'b')
            , (4, 2, Nothing), (6, 2, Just 'c'), (8, 2, Just 'd')
            ] [2, 5, 7, 8, 12])
        [DivNote 'b', DivRest, DivContinue, DivNote 'd', DivRest]

