-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Integrate_test where
import qualified Data.Map as Map

import qualified Util.Ranges as Ranges
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Stream as Stream

import Types


test_integrate_track = do
    let res = DeriveTest.derive_tracks_setup (with_damage [2]) ""
            [ ("> | <", [(0, 1, "")])
            , ("*", [(0, 0, "4c")])
            , ("dyn", [(0, 0, "1")])
            , (">", [(0, 2, "")]) -- appears in normal output, not in integrated
            ]
        extract i = (Derive.integrated_source i, Derive.integrated_events i)
    let [(source, events)] = map extract (Derive.r_integrated res)
    equal source (Right (UiTest.mk_tid 1))
    equal (DeriveTest.extract_levents DeriveTest.e_note (Stream.to_list events))
        ([(0, 1, "4c")], [])
    equal (DeriveTest.extract DeriveTest.e_note res) ([(0, 2, "?")], [])

with_damage :: [TrackNum] -> DeriveTest.Setup
with_damage tracknums = DeriveTest.modify_constant $ \st ->
    st { Derive.state_score_damage = mkdamage tracknums }

mkdamage :: [TrackNum] -> Derive.ScoreDamage
mkdamage tracknums = mempty
    { Derive.sdamage_tracks =
        Map.fromList [(UiTest.mk_tid n, Ranges.range 0 1) | n <- tracknums]
    }
