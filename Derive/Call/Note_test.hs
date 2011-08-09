module Derive.Call.Note_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_random = do
    -- make sure notes in different tracks get different starts
    let extract e = (Score.event_start e, Score.event_duration e)
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks
    let ([e1, e2], []) = run [("start-rnd", [(0, 0, ".1")]),
            (">", [(0, 1, "")]), (">", [(0, 1, "")])]
    check (e1 /= e2)
    equal (fst e1 + snd e1) 1
    equal (fst e2 + snd e2) 1

    let ([e1, e2], []) = run [("dur-rnd", [(0, 0, ".1")]),
            (">", [(0, 1, "")]), (">", [(0, 1, "")])]
    equal (fst e1) (fst e2)
    check (snd e1 /= snd e2)
