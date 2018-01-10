-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.PitchHigh_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.RealTime as RealTime


test_drop = do
    let run pitches dyn = extract $ DeriveTest.derive_tracks ""
            [(">", [(0, 10, "")]), ("*", pitches), ("dyn", dyn)]
        extract = head . (DeriveTest.extract_events $ \e ->
            (DeriveTest.e_nns e, DeriveTest.e_dyn e))
    equal (run [(0, 0, "4c"), (1, 0, "drop 2 2 2")] [(0, 0, "1")])
        ( [(0, 60), (2, 59), (3, 58)]
        , [(0, 1), (1, 1), (2, 0.5), (3, 0)]
        )
    equal (run [(0, 0, "4c"), (1, 0, "drop 2 2 2")] [(0, 0, ".5")])
        ( [(0, 60), (2, 59), (3, 58)]
        , [(0, 0.5), (1, 0.5), (2, 0.25), (3, 0)]
        )

test_drop_noninverted = do
    let run ns ps = DeriveTest.extract DeriveTest.e_dyn $
            DeriveTest.derive_tracks "" [(">", ns), ("*", ps)]
    equal (run [(0, 4, ""), (4, 4, "")]
            [(0, 0, "4c"), (1, 0, "drop 2 2 2"), (4, 4, "4c")])
        ([[(0, 1), (1, 1), (2, 0.5), (3, 0)], [(-RealTime.larger, 1)]], [])

test_drop_lift_note_inverted = do
    let run note = DeriveTest.extract extract $ DeriveTest.derive_tracks ""
            [(">", [(0, 4, note)]), ("*", [(0, 0, "4c")])]
        extract e = (DeriveTest.e_nns e, DeriveTest.e_dyn e)
    -- This verifies that Tags.under_invert works, so that 'drop' goes under
    -- the inversion, while other calls, such as 'd', go above it.
    let ([event], logs) = run "drop 2 2 |"
    equal event
        ( [(0, 60), (3, 59), (4, 58)]
        , [(0, 1), (2, 1), (3, 0.5), (4, 0)]
        )
    equal logs []

    let ([event], logs) = run "d 1 | drop 2 2 |"
    equal event
        ( [(1, 60), (4, 59), (5, 58)]
        , [(0, 1), (3, 1), (4, 0.5), (5, 0)]
        )
    equal logs []

test_drop_lift_note = do
    let run note = DeriveTest.extract extract $ DeriveTest.derive_tracks ""
            [("*", [(0, 0, "4c")]), (">", [(0, 4, note)])]
        extract e = (DeriveTest.e_nns e, DeriveTest.e_dyn e)

    let (events, logs) = run "drop 2 2 |"
    equal events
        [([(0, 60), (3, 59), (4, 58)],
            [(0, 1), (2, 1), (3, 0.5), (4, 0)])]
    equal logs []

    -- Pitch is aligned to the beginning of the fade.
    let (events, logs) = run "drop 2 2 4 |"
    equal events
        [([(0, 60), (1, 59), (2, 58)],
            [(0, 1), (1, 0.75), (2, 0.5), (3, 0.25), (4, 0)])]
    equal logs []

    let (events, logs) = run "drop 2 4 2 |"
    equal events
        [([(0, 60), (1, 59.5), (2, 59), (3, 58.5), (4, 58)],
            [(0, 1), (2, 1), (3, 0.5), (4, 0)])]
    equal logs []

    let (events, logs) = run "drop (5c) 2 2 |"
    equal events
        [([(0, 60), (3, 66), (4, 72)],
            [(0, 1), (2, 1), (3, 0.5), (4, 0)])]
    equal logs []

    -- Pitch aligns to the end of the fade.
    let ([event], logs) = run "Drop 2 2 4 |"
    equal event
        ( [(0, 62), (3, 61), (4, 60)]
        , [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]
        )
    equal logs []

    let ([event], logs) = run "Lift 2 2 4 |"
    equal event
        ( [(0, 58), (3, 59), (4, 60)]
        , [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]
        )
    equal logs []

    let ([event], logs) = run "Drop 2 2 2 | drop 2 2 2 |"
    equal event
        ( [(0, 62), (1, 61), (2, 60), (3, 59), (4, 58)]
        , [(0, 0), (1, 0.5), (2, 1), (3, 0.5), (4, 0)]
        )
    equal logs []

test_approach_dyn = do
    let run pitches = extract $ DeriveTest.derive_tracks ""
            [(">", [(0, 10, "")]), ("*", pitches)]
        extract = head . (DeriveTest.extract_events $ \e ->
            (DeriveTest.e_nns e, DeriveTest.e_dyn e))
    equal (run [(0, 0, "4c"), (2, 0, "ad 2 .5"), (4, 0, "4d")])
        ( [(0, 60), (2, 60), (3, 61), (4, 62)]
        , [(0, 1), (2, 1), (3, 0.75), (4, 1)]
        )
