-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Val_test where
import qualified Util.Texts as Texts
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified Ui.Meter.Meter as Meter
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_env :: Test
test_env = do
    let run = CallTest.run_val
    equal (run Nothing "env x")
        (Nothing, ["environ val not found: \"x\""])
    equal (run Nothing "env x 42") (Just (DeriveT.num 42), [])
    equal (run (Just "x = 42") "env x") (Just (DeriveT.num 42), [])
    equal (run (Just "x = 42") "env x str")
        (Nothing, ["env \"x\" expected Str but got Signal"])

test_prev_next_val :: Test
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

test_linear_next :: Test
test_linear_next = do
    let run extract track =
            DeriveTest.extract extract $ DeriveTest.derive_tracks ""
                [(">", [(0, 4, "")]), track]
    let (result, logs) = run DeriveTest.e_dyn
            ("dyn", [(0, 0, "xcut (i> 0 1) (i> 1 0) 2"), (4, 0, "0")])
    equal logs []
    equal result
        [ [ (0, 0), (0.5, 1/8), (0.5, 7/8), (1, 6/8), (1, 2/8)
          , (1.5, 3/8), (1.5, 5/8), (2, 4/8), (2.5, 5/8), (2.5, 3/8)
          , (3, 2/8), (3, 6/8), (3.5, 7/8), (3.5, 1/8)
          , (4, 0)
          ]
        ]

    let (result, logs) = run DeriveTest.e_nns
            ("*"
            , [(0, 0, "xcut (i> (4c) (5c)) (i> (5c) (4c)) 2"), (2, 0, "2c")]
            )
    equal logs []
    equal result
        [ [ (0,   60), (0.5, 63)
          , (0.5, 69), (1, 66)
          ,            (1.5, 69)
          , (1.5, 63), (2, 60)
          , (2, NN.c2)
          ]
        ]
    strings_like
        (snd $ run DeriveTest.e_dyn ("dyn", [(0, 0, "xcut (i> 0 (4c))")]))
        ["arg 2/bp: expected Signal but got Pitch"]
    strings_like (snd $ run DeriveTest.e_dyn ("*", [(0, 0, "xcut (i> hi)")]))
        ["arg 1/bp: expected Signal or Pitch"]

test_down_from :: Test
test_down_from = do
    let run = DeriveTest.extract DeriveTest.e_dyn . DeriveTest.derive_tracks ""
    equal (run [(">", [(0, 4, "dyn=(df 2 1) |")])]) ([[(0, 2), (2, 0)]], [])

test_timestep :: Test
test_timestep = do
    let run start vcall = DeriveTest.extract extract $
            DeriveTest.derive_tracks_setup (DeriveTest.with_default_ruler ruler)
                "" [(">", [(start, 0, ("d (ts " <> vcall <> ") |"))])]
        extract = Score.event_start
        ruler = UiTest.mkruler_ranks $ zip [0, 1, 2, 3, 4, 6, 8, 10, 12]
            (cycle (map fromEnum [Meter.W, Meter.Q, Meter.Q, Meter.Q]))
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

test_signal :: Test
test_signal = do
    let run = CallTest.run_val Nothing
    let mksig typ = DeriveT.VSignal . ScoreT.Typed typ . Signal.from_pairs
    equal (run "signal") (Just (mksig ScoreT.Untyped []), [])
    equal (run "signal 0 2") (Just (mksig ScoreT.Untyped [(0, 2)]), [])
    equal (run "signal d 0 2") (Just (mksig ScoreT.Diatonic [(0, 2)]), [])
    let roundtrip sig =
            ( run (strip_parens (ShowVal.show_val sig))
            , (Just sig, [])
            )
    uncurry equal (roundtrip (mksig ScoreT.Untyped [(0, 2)]))
    uncurry equal (roundtrip (mksig ScoreT.Nn [(1, 2), (3, 4)]))
    pprint (ShowVal.show_val (mksig ScoreT.Chromatic [(0, 2)]))
    pprint (ShowVal.show_val (mksig ScoreT.Nn [(0, 2), (2, 4)]))

strip_parens :: Text -> Text
strip_parens = Texts.dropPrefix "(" . Texts.dropSuffix ")"

test_psignal :: Test
test_psignal = do
    let run = first (fmap un_psig) . CallTest.run_val Nothing
    let mksig = DeriveT.VPSignal . PSignal.from_pairs
            . map (fmap PSignal.nn_pitch)
    equal (run "psignal") (Just [], [])
    equal (run "psignal 0 (4c)") (Just [(0, Right NN.c4)], [])
    -- These won't be parseable so no roundtrip for this one.
    pprint (ShowVal.show_val (mksig [(0, 2)]))
    pprint (ShowVal.show_val (mksig [(0, 2), (2, 4)]))

un_psig :: DeriveT.Val
    -> [(RealTime, Either PSignal.PitchError Pitch.NoteNumber)]
un_psig =
    map (second (DeriveT.pitch_nn . DeriveT.coerce))
        . PSignal.to_pairs . expect_psig
    where
    expect_psig (DeriveT.VPSignal sig) = sig
    expect_psig val = error $ "expected VPSignal: "
        <> untxt (ShowVal.show_val val)
