-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.ControlFunction_test where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Cmd.Ruler.Meter as Meter
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import Global


test_cf_rnd = do
    let run sus notes = DeriveTest.extract extract $ DeriveTest.derive_tracks ""
            [("> | %sus = " <> sus, [(n, 1, "") | n <- Seq.range' 0 notes 1])]
        extract = Score.event_duration
    equal (run ".5" 1) ([0.5], [])
    let (durs, logs) = run "(cf-rnd .5 1.5)" 5
    equal logs []
    check ("not the same: " <> pretty durs) $ not (all (== head durs) durs)
    check ("in range 0.8--1.2: " <> pretty durs) $
        all (Num.inRange 0.8 1.2) durs

test_cf_swing = do
    let run marks amount tracks events = DeriveTest.extract Score.event_start $
            DeriveTest.derive_tracks_setup (with_ruler marks)
                    "apply-start-offset" $
                tracks ++ [("> | %start-s = (cf-swing q " <> amount <> ")",
                    [(n, 0, "") | n <- events])]
        with_ruler = DeriveTest.with_default_ruler . UiTest.ruler
            . map (second Meter.name_to_rank)

    let marks = take 8 $ zip (Seq.range_ 0 2)
            [Meter.H, Meter.Q, Meter.Q, Meter.Q]
        events = [0, 1, 2, 3]
    equal (run marks ".5" [] events) ([0, 1.5, 2, 3.5], [])
    equal (run marks "-.5" [] events) ([0, 0.5, 2, 2.5], [])
    equal (run marks "%swing" [("swing", [(0, 0, "0"), (2, 0, ".5")])] events)
        ([0, 1, 2, 3.5], [])
