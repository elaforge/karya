{-# LANGUAGE ScopedTypeVariables #-}
module Derive.Call.Util_test where
import Util.Test
import qualified Ui.State as State
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.TrackLang as TrackLang


test_random = do
    let run (seed :: Double) = DeriveTest.eval State.empty
            . Derive.with_val TrackLang.v_seed seed
        rand = Util.random :: Derive.Deriver Int
    equal (run 0 rand) (run 0 rand)
    check (run 0 rand /= run 1 rand)
    check $ run 2 (Internal.with_stack_region 0 1 rand)
        /= run 2 (Internal.with_stack_region 1 1 rand)
    equal (run 0 (Util.shuffle ['a'..'f'])) (Right "edbacf") -- ya ya ya :)


test_c_equal = do
    -- Test the '=' call, but also test the special parsing Derive.Note deriver
    -- eval in general.
    let run title evts = DeriveTest.extract extract $
            DeriveTest.derive_tracks [(title, evts)]
        extract e = (s, inst, attrs)
            where (s, _, _, inst, attrs) = DeriveTest.e_everything e

    -- log stack should be at the track level
    let (evts, logs) = run "> | inst = inst" [(0, 1, "")]
    equal evts []
    strings_like logs ["expected Instrument"]

    -- only the event with the error is omitted
    let (evts, logs) = run ">" [(0, 1, "inst = inst |"), (1, 1, "")]
    equal evts [(1, Nothing, [])]
    strings_like logs ["expected Instrument"]

    equal (run ">i" [(0, 1, ""), (1, 1, "inst = >i2 |"), (2, 1, "n >i3 |")])
        ([(0, Just "i", []), (1, Just "i2", []), (2, Just "i3", [])], [])

