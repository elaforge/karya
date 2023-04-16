-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.ControlFunction_test where
import qualified Data.Maybe as Maybe

import qualified Util.Num as Num
import qualified Util.Lists as Lists
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Controls as Controls
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Signal as Signal
import qualified Ui.Meter.Meter as Meter
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_cf_rnd :: Test
test_cf_rnd = do
    let run title notes = DeriveTest.extract extract $
            DeriveTest.derive_tracks ""
                [(title, [(n, 1, "") | n <- Lists.range' 0 notes 1])]
        extract = Score.event_duration
    equal (run "> | sus = .5" 1) ([0.5], [])
    let (durs, logs) = run "> | sus = (cf-rnd .5 1.5)" 5
    equal logs []
    check ("not the same: " <> pretty durs) $ not (all (== head durs) durs)
    check ("in range 0.5--1.5: " <> pretty durs) $
        all (Num.inRange 0.5 1.5) durs
    pprint durs
    let (durs, logs) = run "> | sus = 1 | sus = (cf-rnd+ .5 1.5)" 5
    equal logs []
    check ("in range 1.5--2.5: " <> pretty durs) $
        all (Num.inRange 1.5 2.5) durs
    pprint durs

test_cf_backed :: Test
test_cf_backed = do
    let run title = DeriveTest.extract extract $
            DeriveTest.derive_tracks title
                [ ("dyn", [(0, 0, ".5")])
                , (">", [(0, 1, ""), (1, 1, "")])
                ]
        extract :: Score.Event -> Maybe Double
        extract = DeriveTest.e_environ_val EnvKey.dynamic_val
    let (dyns, logs) = first Maybe.catMaybes $ run "dyn = (cf-rnd01 -.25 .25)"
    equal logs []
    equal (length dyns) 2
    check ("not the same: " <> pretty dyns) $ not (all (== head dyns) dyns)
    check ("in range 0.25--.75: " <> pretty dyns) $
        all (Num.inRange 0.25 0.75) dyns
    pprint dyns

test_cf_rnd_transformer :: Test
test_cf_rnd_transformer = do
    -- A transformer should create a function with random based on position.
    let run notes = DeriveTest.extract extract $
            DeriveTest.derive_tracks_setup trans "c = (cf-rnd 1 2) | t"
                [(">", [(n, 1, "") | n <- Lists.range' 0 notes 1])]
        trans = CallTest.with_note_transformer "t" $ CallTest.transformer $
            \_args deriver -> do
                c_at <- fmap ScoreT.typed_val $
                    Typecheck.resolve_function $ DeriveT.Ref "c" Nothing
                fmap (set_dyn c_at) <$> deriver
        set_dyn c_at event = Score.set_control Controls.dynamic
            (ScoreT.untyped (Signal.constant (c_at (Score.event_start event))))
            event
        extract = Score.initial_dynamic
    let (dyns, logs) = run 4
    equal logs []
    check ("not the same: " <> pretty dyns) $ not (all (== head dyns) dyns)
    check ("in range 1--2: " <> pretty dyns) $ all (Num.inRange 1 2) dyns

test_cf_swing :: Test
test_cf_swing = do
    let run marks amount tracks events = DeriveTest.extract Score.event_start $
            DeriveTest.derive_tracks_setup (with_ruler marks)
                    "apply-start-offset" $
                tracks ++
                [ ( "> | start-s = (cf-swing q " <> amount <> ")"
                  , [(n, 0, "") | n <- events]
                  )
                ]
        with_ruler = DeriveTest.with_default_ruler . UiTest.mkruler_ranks
            . map (second fromEnum)

    let marks = take 8 $ zip (Lists.range_ 0 2)
            [Meter.H, Meter.Q, Meter.Q, Meter.Q]
        events = [0, 1, 2, 3]
    equal (run marks ".5" [] events) ([0, 1.5, 2, 3.5], [])
    equal (run marks "-.5" [] events) ([0, 0.5, 2, 2.5], [])
    equal (run marks "%swing" [("swing", [(0, 0, "0"), (2, 0, ".5")])] events)
        ([0, 1, 2, 3.5], [])
