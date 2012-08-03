module Derive.Call.Integrate_test where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Ranges as Ranges
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent

import Types


test_integrate_track = do
    let res = DeriveTest.derive_tracks_with (with_damage [2])
            [ ("> | <", [(0, 1, "")])
            , ("*", [(0, 0, "4c")])
            , ("dyn", [(0, 0, "1")])
            , (">", [(0, 2, "")]) -- appears in normal output, not in integrated
            ]
        extract i = (Derive.integrated_source i, Derive.integrated_events i)
    let [(source, events)] = map extract (Derive.r_integrated res)
    equal source (Right (UiTest.mk_tid 1))
    equal (map (fmap DeriveTest.e_note2) events) [LEvent.Event (0, 1, "4c")]
    equal (DeriveTest.extract DeriveTest.e_note2 res) ([(0, 2, "?")], [])

with_damage :: [TrackNum] -> Derive.Deriver a -> Derive.Deriver a
with_damage tracknums = DeriveTest.modify_constant $ \st ->
    st { Derive.state_score_damage = mkdamage tracknums }

mkdamage tracknums = mempty
    { Derive.sdamage_tracks =
        Map.fromList [(UiTest.mk_tid n, Ranges.range 0 1) | n <- tracknums]
    }
