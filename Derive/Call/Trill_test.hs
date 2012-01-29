module Derive.Call.Trill_test where
import Util.Test
import qualified Ui.State as State
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Trill as Trill
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.Signal as Signal


test_trill = do
    let run tracks = extract $ DeriveTest.derive_tracks $
            (">", [(0, 3, "")]) : tracks
        extract = DeriveTest.extract DeriveTest.e_pitch
    -- Defaults to diatonic.
    equal (run [("*twelve", [(0, 0, "tr (4c) 1 1")])])
        ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run [("*twelve", [(0, 0, "tr (4c) 1c 1")])])
        ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run [("*twelve", [(0, 0, "tr (4c) 1d 1")])])
        ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run [("*twelve", [(0, 0, "tr (4c) -1d 1")])])
        ([[(0, 60), (1, 59), (2, 60)]], [])

    let run_c suffix val = run
            [ ("trill-neighbor" ++ suffix, [(0, 0, val)])
            , ("*twelve", [(0, 0, "tr (4c) _ 1")])
            ]
    equal (run_c "" "1") ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run_c "" "-2") ([[(0, 60), (1, 57), (2, 60)]], [])
    equal (run_c ":d" "1") ([[(0, 60), (1, 62), (2, 60)]], [])
    equal (run_c ":d" "-2") ([[(0, 60), (1, 57), (2, 60)]], [])
    equal (run_c ":c" "1") ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run_c ":c" "-2") ([[(0, 60), (1, 58), (2, 60)]], [])

test_absolute_trill = do
    let f = Trill.absolute_trill (0, 1)
        run = extract . DeriveTest.run State.empty
        extract = DeriveTest.extract_run Signal.unsignal
    equal (run $ f (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0)]
    equal (run $ f (con (-1)) (con 2)) $
        Right [(0, 0), (0.5, -1), (1, 0)]
    -- Cycles must be integral.
    equal (run $ f (con 1) (con 1.9)) $
        Right [(0, 0)]

    -- Trill speed is in real time and not affected by stretch.
    equal (run $ Derive.d_stretch 2 $ f (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0), (1.5, 1), (2, 0)]

    -- It still produces an integral number of cycles.
    equal (run $ Derive.d_stretch 1.8 $ f (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0)]

    -- Changing depth signal.
    equal (run $ f (Signal.signal [(0, 1), (0.5, 2)]) (con 4)) $
        Right [(0, 0), (0.25, 1), (0.5, 0), (0.75, 2), (1, 0)]
    equal (run $ f (con 1) (Signal.signal [(0, 2), (0.5, 4)])) $
        Right [(0, 0), (0.5, 1), (0.75, 0)]

test_score_trill = do
    let f dur = Trill.score_trill (0, dur)
        run = extract . DeriveTest.run State.empty
        extract = DeriveTest.extract_run Signal.unsignal
    equal (run $ f 1 (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0)]

    -- If the event was length 2 there should be 2 cycles
    equal (run $ f 2 (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0), (1.5, 1), (2, 0)]

    -- Ignores a bit of extra times since cycles must be integral.
    equal (run $ f 2.5 (con 1) (con 2)) $
        Right [(0, 0), (0.5, 1), (1, 0), (1.5, 1), (2, 0)]

    -- Trill speed affected by stretch.
    equal (run $ Derive.d_stretch 2 $ f 1 (con 1) (con 2)) $
        Right [(0, 0), (1, 1), (2, 0)]

con = Signal.constant

-- * pitch calls

test_pitch_absolute_trill = do
    equal (CallTest.run_pitch [(0, "abs-trill (4e) 2 2"), (2.8, "4c")]) $
        zip [0, 0.5, 1, 1.5, 2] (cycle [64, 66]) ++ [(2.8, 60)]
