module Derive.Call.Misc_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_multiple = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks ""
        extract e = (Score.event_start e, DeriveTest.e_inst e)
    equal (run
            [("> | multiple \"(inst = >s/1) \"(inst = >s/2)", [(0, 1, "")])])
        ([(0, "s/1"), (0, "s/2")], [])
    equal (run [("> | multiple >s/1 >s/2", [(0, 1, "")])])
        ([(0, "s/1"), (0, "s/2")], [])
