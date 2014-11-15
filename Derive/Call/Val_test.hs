-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Val_test where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Cmd.Meter as Meter
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.NN as NN
import Global


test_env = do
    let run = CallTest.run_val
    equal (run Nothing "e x") (Nothing, ["Error: environ val not found: x"])
    equal (run Nothing "e x 42") (Just (TrackLang.num 42), [])
    equal (run (Just "x = 42") "e x") (Just (TrackLang.num 42), [])
    equal (run (Just "x = 42") "e x str")
        (Nothing, ["Error: env x expected Symbol but got Num"])

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
    let f extract track =
            DeriveTest.extract extract $ DeriveTest.derive_tracks ""
                [(">", [(0, 4, "")]), track]
    let (result, logs) = f DeriveTest.e_dyn
            ("dyn", [(0, 0, "xcut (i> 0 1) (i> 1 0) 2"), (4, 0, "0")])
    equal logs []
    equal result [[(0, 0), (0.5, 1), (1, 0.25), (1.5, 0.75),
        (2, 0.5), (2.5, 0.5), (3, 0.75), (3.5, 0.25), (4, 0)]]

    let (result, logs) = f DeriveTest.e_nns ("*",
            [(0, 0, "xcut (i> (4c) (5c)) (i> (5c) (4c)) 2"), (4, 0, "2c")])
    equal logs []
    equal result [[(0, 60), (0.5, 72), (1, 63), (1.5, 69),
        (2, 66), (2.5, 66), (3, 69), (3.5, 63), (4, NN.c2)]]
    strings_like
        (snd $ f DeriveTest.e_dyn ("dyn", [(0, 0, "xcut (i> 0 (4c))")]))
        ["arg 2/bp: expected Num but got Pitch"]
    strings_like (snd $ f DeriveTest.e_dyn ("*", [(0, 0, "xcut (i> >hi)")]))
        ["arg 1/bp: expected Num or Pitch"]

test_timestep = do
    let run start vcall = DeriveTest.extract extract $
            DeriveTest.derive_tracks_with_ui id (DeriveTest.set_ruler ruler)
                "" [(">", [(start, 0, ("d (ts " <> vcall <> ") |"))])]
        extract = Score.event_start
        ruler = UiTest.ruler $ zip [0, 1, 2, 3, 4, 6, 8, 10, 12]
            (cycle [Meter.r_1, Meter.r_4, Meter.r_4, Meter.r_4])
    let (evts, logs) = run 0 "'r:z'"
    equal evts []
    strings_like logs ["expected Symbol"]

    -- Whole note in the first measure is 4.
    equal (run 0 "w") ([4], [])
    equal (run 1 "w") ([5], [])

    -- Whole note in the second measure is 8.
    equal (run 4 "w") ([12], [])
    equal (run 6 "w") ([14], [])
    equal (run 4 "q") ([6], []) -- quarter is 2

    equal (run 0 "q") ([1], [])
    equal (run 0 "q .5") ([0.5], [])
    -- It's at the end of the ruler so it can't go forwards.  Take the
    -- backwards distance instead.
    equal (run 12 "q") ([14], [])
    -- Whole step works even though there isn't a whole step left.
    equal (run 10 "w") ([18], [])

    -- TODO should be an error, there are no sixteenths
    equal (run 0 "s") ([1], [])

test_cf_rnd = do
    let run sus notes = DeriveTest.extract extract $ DeriveTest.derive_tracks ""
            [("> | %sus = " <> sus, [(n, 1, "") | n <- Seq.range' 0 notes 1])]
        extract = Score.event_duration
    equal (run ".5" 1) ([0.5], [])
    let (durs, logs) = run "(cf-rnd .5 1.5)" 5
    equal logs []
    check $ not (all (== head durs) durs)
    check (all (Num.in_range 0.8 1.2) durs)

test_cf_swing = do
    let run marks amount tracks events = DeriveTest.extract Score.event_start $
            DeriveTest.derive_tracks_with_ui id (with_ruler marks)
                    "apply-start-offset" $
                tracks ++ [("> | %start-s = (cf-swing q " <> amount <> ")",
                    [(n, 0, "") | n <- events])]
        with_ruler = DeriveTest.set_ruler . UiTest.ruler
            . map (second Meter.name_to_rank)

    let marks = take 8 $ zip (Seq.range_ 0 2)
            [Meter.H, Meter.Q, Meter.Q, Meter.Q]
        events = [0, 1, 2, 3]
    equal (run marks ".5" [] events) ([0, 1.5, 2, 3.5], [])
    equal (run marks "-.5" [] events) ([0, 0.5, 2, 2.5], [])
    equal (run marks "%swing" [("swing", [(0, 0, "0"), (2, 0, ".5")])] events)
        ([0, 1, 2, 3.5], [])
