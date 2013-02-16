{-# LANGUAGE TupleSections #-}
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
    let present = [1, 2, 3, 5]
    equal (run "omit .5 |") (map (, 1) present, [])

    -- Ensure different calls to the same block are differently random.
    let blocks ns = extract $ DeriveTest.derive_blocks
            [ ("top", [(">", [(p, 1, n) | (p, n) <- zip (Seq.range_ 0 1) ns])])
            , ("sub=ruler", [(">", [(0, 1, "omit .5 |")])])
            ]
    let present = [2, 3, 6, 8, 9]
    equal (blocks (replicate 10 "sub")) ([(n, 1) | n <- present], [])

test_alternate = do
    let run s = DeriveTest.extract DeriveTest.e_pitch $ DeriveTest.derive_blocks
            [ ("top", [(">", [(p, 1, s) | p <- Seq.range 0 5 1])])
            , ("s1=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
            , ("s2=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4d")])])
            ]
    equal (run "alt 's1' 's2'")
        (["4d", "4c", "4d", "4c", "4c", "4c"], [])
    let (ps, logs) = run "alt 'bad (call' 's2'"
    equal ps $ replicate 2 "4d"
    strings_like logs $ replicate 4 "parse error"
