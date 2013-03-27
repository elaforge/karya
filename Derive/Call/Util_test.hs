{-# LANGUAGE ScopedTypeVariables #-}
module Derive.Call.Util_test where
import Util.Test
import qualified Ui.State as State
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import Types


test_random = do
    let f seed = DeriveTest.extract extract $ DeriveTest.derive_blocks
            [ ("top", [(">", [(0, 1, seed ++ "b"), (1, 1, seed ++ "b")])])
            , ("b", [(">", [(0, 1, "")]), ("c", [(0, 0, "range")])])
            ]
        extract = DeriveTest.e_control "c"

    -- Different calls to the same block are differently random.
    let ([[(_, v1)], [(_, v2)]], logs) = f ""
    equal logs []
    check $ v1 /= v2

    -- Unless overridden.  Note that the seed is set after the difference in
    -- position, so these calls should be the same.
    let ([[(_, v1)], [(_, v2)]], logs) = f "seed = 1 | "
    equal logs []
    equal v1 v2

test_randoms_in = do
    let run seed = expect_right "run" . DeriveTest.eval State.empty
            . Derive.with_val TrackLang.v_seed (seed :: Int)
        randoms seed low high = take 4 $
            run seed (Util.randoms_in low high)
    let double :: Int -> Double -> Double -> [Double]
        double = randoms
        int :: Int -> Int -> Int -> [Int]
        int = randoms
    -- Just make sure I get numbers that look like they're in the right range.
    equal (map round $ double 0 0 100) [16, 99, 4, 60]
    equal (map round $ double 0 (-100) 100) [-68, 98, -92, 19]
    equal (int 0 0 100) [94, 51, 33, 62]
    equal (int 1 (-100) 100) [-72, 62, -70, 46]

test_c_equal = do
    -- Test the '=' call, but also test the special parsing Derive.Note deriver
    -- eval in general.
    let run title evts = DeriveTest.extract e_inst $
            DeriveTest.derive_tracks [(title, evts)]

    -- log stack should be at the track level
    let (evts, logs) = run "> | inst = inst" [(0, 1, "")]
    equal evts []
    strings_like logs ["expected Instrument"]

    -- only the event with the error is omitted
    let (evts, logs) = run ">" [(0, 1, "inst = inst |"), (1, 1, "")]
    equal evts [(1, "")]
    strings_like logs ["expected Instrument"]

    equal (run ">i" [(0, 1, ""), (1, 1, "inst = >i2 |"), (2, 1, "n >i3 |")])
        ([(0, "i"), (1, "i2"), (2, "i3")], [])

test_c_equal_note_transformer = do
    let run events = DeriveTest.extract e_inst $
            DeriveTest.linear_derive_tracks id
                [ (">", events)
                , (">", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
                ]
    equal (run []) ([(0, ""), (1, ""), (2, "")], [])
    equal (run [(0, 2, "inst = >i")]) ([(0, "i"), (1, "i"), (2, "")], [])
    equal (run [(0, 3, "inst = >i")]) ([(0, "i"), (1, "i"), (2, "i")], [])
    equal (run [(0, 1, "inst = >i1"), (1, 1, "inst = >i2")])
        ([(0, "i1"), (1, "i2"), (2, "")], [])

e_inst :: Score.Event -> (RealTime, String)
e_inst e = (Score.event_start e, Score.inst_name (Score.event_instrument e))
