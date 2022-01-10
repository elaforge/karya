-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.ControlFunction_test where
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Call as Call
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Controls as Controls
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Signal as Signal
import qualified Ui.Meter.Meter as Meter
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_cf_rnd = do
    let run sus notes = DeriveTest.extract extract $ DeriveTest.derive_tracks ""
            [("> | %sus = " <> sus, [(n, 1, "") | n <- Seq.range' 0 notes 1])]
        extract = Score.event_duration
    equal (run ".5" 1) ([0.5], [])
    let (durs, logs) = run "(cf-rnd .5 1.5)" 5
    equal logs []
    check ("not the same: " <> pretty durs) $ not (all (== head durs) durs)
    check ("in range 0.5--1.5: " <> pretty durs) $
        all (Num.inRange 0.5 1.5) durs
    pprint durs

test_cf_rnd_transformer = do
    -- A transformer should create a function with random based on position.
    let run notes = DeriveTest.extract extract $
            DeriveTest.derive_tracks_setup trans "%c = (cf-rnd 1 2) | t"
                [(">", [(n, 1, "") | n <- Seq.range' 0 notes 1])]
        trans = CallTest.with_note_transformer "t" $ CallTest.transformer $
            \_args deriver -> do
                c_at <- Call.to_function $ DeriveT.LiteralControl "c"
                fmap (set_dyn c_at) <$> deriver
        set_dyn c_at event = Score.set_control Controls.dynamic
            (ScoreT.untyped (Signal.constant (c_at (Score.event_start event))))
            event
        extract = Score.initial_dynamic
    let (dyns, logs) = run 4
    equal logs []
    check ("not the same: " <> pretty dyns) $ not (all (== head dyns) dyns)
    check ("in range 1--2: " <> pretty dyns) $ all (Num.inRange 1 2) dyns

test_cf_swing = do
    let run marks amount tracks events = DeriveTest.extract Score.event_start $
            DeriveTest.derive_tracks_setup (with_ruler marks)
                    "apply-start-offset" $
                tracks ++ [("> | %start-s = (cf-swing q " <> amount <> ")",
                    [(n, 0, "") | n <- events])]
        with_ruler = DeriveTest.with_default_ruler . UiTest.mkruler_ranks
            . map (second fromEnum)

    let marks = take 8 $ zip (Seq.range_ 0 2)
            [Meter.H, Meter.Q, Meter.Q, Meter.Q]
        events = [0, 1, 2, 3]
    equal (run marks ".5" [] events) ([0, 1.5, 2, 3.5], [])
    equal (run marks "-.5" [] events) ([0, 0.5, 2, 2.5], [])
    equal (run marks "%swing" [("swing", [(0, 0, "0"), (2, 0, ".5")])] events)
        ([0, 1, 2, 3.5], [])
