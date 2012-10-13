module Derive.Call.Random_test where
import Util.Test
import qualified Util.Seq as Seq
import qualified Derive.DeriveTest as DeriveTest


test_alternate = do
    let run s = DeriveTest.extract DeriveTest.e_twelve $ DeriveTest.derive_blocks
            [ ("top", [(">", [(n, 1, s) | n <- Seq.range 0 5 1])])
            -- [ ("top", [(">", [(n, 1, "s1") | n <- Seq.range 0 5 1])])
            , ("s1=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])])
            , ("s2=ruler", [(">", [(0, 1, "")]), ("*", [(0, 0, "4d")])])
            ]

    -- Yes I'm testing "randomness", but it should be consistent.
    equal (run "alt 's1' 's2'")
        (["4c", "4c", "4d", "4d", "4d", "4d"], [])
    equal (run "seed = 42 | alt 's1' 's2'")
        (["4d", "4c", "4c", "4c", "4c", "4d"], [])
    let (ps, logs) = run "alt 'bad (call' 's2'"
    equal ps ["4d", "4d", "4d", "4d"]
    strings_like logs ["parse error", "parse error"]
