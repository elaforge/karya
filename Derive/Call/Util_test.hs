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
