-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Val_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Ruler.Meter as Meter
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.BaseTypes as BaseTypes

import qualified Perform.NN as NN
import Global


test_env = do
    let run = CallTest.run_val
    equal (run Nothing "e x") (Nothing, ["Error: environ val not found: \"x\""])
    equal (run Nothing "e x 42") (Just (BaseTypes.num 42), [])
    equal (run (Just "x = 42") "e x") (Just (BaseTypes.num 42), [])
    equal (run (Just "x = 42") "e x str")
        (Nothing, ["Error: env \"x\" expected Str but got Num"])

test_prev_next_val = do
    let runc control = DeriveTest.extract (DeriveTest.e_control "c") $
            DeriveTest.derive_tracks "" [(">", [(0, 10, "")]), ("c", control)]
    let runp notes pitch = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_tracks "" [(">", notes), ("*", pitch)]

    -- prev
    equal (runc [(0, 0, ".5"), (1, 0, "set (<)")])
        ([[(0, 0.5), (1, 0.5)]], [])
    equal (runp [(0, 1, ""), (1, 1, "")] [(0, 0, "4a"), (1, 0, "set (<)")])
        ([(0, 1, "4a"), (1, 1, "4a")], [])
    -- Within a note works though.
    equal (runp [(0, 2, "")] [(0, 0, "4a"), (1, 0, "set (<)")])
        ([(0, 2, "4a")], [])
    equal (runp [(0, 3, "")]
            [(0, 0, "4a"), (1, 0, "set (<)"), (2, 0, "set (<)")])
        ([(0, 3, "4a")], [])

    -- next
    equal (runc [(0, 0, "set (>)"), (1, 0, ".75")])
        ([[(0, 0.75), (1, 0.75)]], [])
    -- Next also works, because the next event is always included.
    equal (runp [(0, 1, ""), (1, 1, "")] [(0, 0, "set (>)"), (1, 0, "4a")])
        ([(0, 1, "4a"), (1, 1, "4a")], [])

test_linear_next = do
    let run extract track =
            DeriveTest.extract extract $ DeriveTest.derive_tracks ""
                [(">", [(0, 4, "")]), track]
    let (result, logs) = run DeriveTest.e_dyn
            ("dyn", [(0, 0, "xcut (i> 0 1) (i> 1 0) 2"), (4, 0, "0")])
    equal logs []
    equal result [[(0, 0), (0.5, 1), (1, 0.25), (1.5, 0.75),
        (2, 0.5), (2.5, 0.5), (3, 0.75), (3.5, 0.25), (4, 0)]]

    let (result, logs) = run DeriveTest.e_nns ("*",
            [(0, 0, "xcut (i> (4c) (5c)) (i> (5c) (4c)) 2"), (4, 0, "2c")])
    equal logs []
    equal result [[(0, 60), (0.5, 72), (1, 63), (1.5, 69),
        (2, 66), (2.5, 66), (3, 69), (3.5, 63), (4, NN.c2)]]
    strings_like
        (snd $ run DeriveTest.e_dyn ("dyn", [(0, 0, "xcut (i> 0 (4c))")]))
        ["arg 2/bp: expected Num but got Pitch"]
    strings_like (snd $ run DeriveTest.e_dyn ("*", [(0, 0, "xcut (i> hi)")]))
        ["arg 1/bp: expected Num or Pitch"]

test_down_from = do
    let run = DeriveTest.extract DeriveTest.e_dyn . DeriveTest.derive_tracks ""
    equal (run [(">", [(0, 4, "%dyn=(df 2 1) |")])])
        ([[(0, 2), (1, 1), (2, 0)]], [])

test_timestep = do
    let run start vcall = DeriveTest.extract extract $
            DeriveTest.derive_tracks_setup (DeriveTest.with_default_ruler ruler)
                "" [(">", [(start, 0, ("d (ts " <> vcall <> ") |"))])]
        extract = Score.event_start
        ruler = UiTest.ruler $ zip [0, 1, 2, 3, 4, 6, 8, 10, 12]
            (cycle [Meter.r_1, Meter.r_4, Meter.r_4, Meter.r_4])
    let (evts, logs) = run 0 "'r:z'"
    equal evts []
    strings_like logs ["expected Str"]

    -- Whole note in the first measure is 4.
    equal (run 0 "w") ([4], [])
    equal (run 1 "w") ([5], [])

    -- Whole note in the second measure is 8.
    equal (run 4 "w") ([12], [])
    equal (run 6 "w") ([14], [])
    equal (run 4 "q") ([6], []) -- quarter is 2

    equal (run 0 "q") ([1], [])
    equal (run 0 "q 2") ([2], [])
    equal (run 0 "q 5") ([6], [])
    -- It's at the end of the ruler so it can't go forwards.  Take the
    -- backwards distance instead.
    equal (run 12 "q") ([14], [])
    -- Whole step works even though there isn't a whole step left.
    equal (run 10 "w") ([18], [])

    -- TODO should be an error, there are no sixteenths
    equal (run 0 "s") ([1], [])
