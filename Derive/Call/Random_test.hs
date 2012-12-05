module Derive.Call.Random_test where
import Util.Test
import qualified Util.Seq as Seq
import qualified Derive.DeriveTest as DeriveTest


test_omit = do
    let extract = DeriveTest.extract DeriveTest.e_start_dur
    let run n = extract $ DeriveTest.derive_tracks
            [(">", [(p, 1, n) | p <- Seq.range 0 5 1])]
    equal (run "omit 0 |") ([(p, 1) | p <- Seq.range 0 5 1], [])
    equal (run "omit 1 |") ([], [])
    equal (run "omit .5 |") ([(0, 1), (2, 1), (3, 1), (4, 1)], [])

    -- Ensure different calls to the same block are differently random.
    let blocks ns = extract $ DeriveTest.derive_blocks
            [ ("top", [(">", [(p, 1, n) | (p, n) <- zip (Seq.range_ 0 1) ns])])
            , ("sub=ruler", [(">", [(0, 1, "omit .5 |")])])
            ]
    equal (blocks (replicate 10 "sub"))
        ([(1, 1), (3, 1), (4, 1), (6, 1), (9, 1)], [])

test_alternate = do
    let run s = DeriveTest.extract DeriveTest.e_twelve $
            DeriveTest.derive_blocks
            [ ("top", [(">", [(p, 1, s) | p <- Seq.range 0 5 1])])
            , ("s1=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
            , ("s2=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4d")])])
            ]

    -- Yes I'm testing "randomness", but it should be consistent.
    equal (run "alt 's1' 's2'")
        (["4c", "4d", "4d", "4c", "4d", "4c"], [])
    equal (run "seed = 42 | alt 's1' 's2'")
        (["4c", "4c", "4d", "4d", "4c", "4d"], [])
    let (ps, logs) = run "alt 'bad (call' 's2'"
    equal ps ["4d", "4d", "4d"]
    strings_like logs ["parse error", "parse error", "parse error"]
