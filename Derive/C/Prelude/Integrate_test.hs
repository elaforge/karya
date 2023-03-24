-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Integrate_test where
import qualified Data.Map as Map

import qualified Util.Ranges as Ranges
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Stream as Stream

import qualified Ui.UiTest as UiTest

import           Types
import           Util.Test


test_integrate_track :: Test
test_integrate_track = do
    let res = DeriveTest.derive_tracks_setup (with_damage [2]) ""
            [ ("c1", [(0, 0, ".5")])
            , ("c2", [(0, 0, ".75")])
            , (">i2 | < (list c2)", [(0, 1, "")])
            , ("*", [(0, 0, "4c")])
            , (">i2", [(0, 2, "")]) -- appears in non-integrated output
            ]
        extract i = (Derive.integrated_source i, Derive.integrated_events i)
    let [(source, integrated)] = map extract (Derive.r_integrated res)
    equal source (Right (UiTest.mk_tid 3))
    let e_note e =
            ( DeriveTest.e_note e
            , filter ((/= Controls.dynamic_integrate) . fst) $
                DeriveTest.e_controls e
            )
    equal (DeriveTest.extract_levents e_note (Stream.to_list integrated))
        ( [((0, 1, "4c"), [("c2", [(0, 0.75)])])]
        , []
        )
    equal (DeriveTest.extract
            (\e -> (DeriveTest.e_note e, DeriveTest.e_instrument e)) res)
        ([((0, 2, "?"), "i2")], [])

with_damage :: [TrackNum] -> DeriveTest.Setup
with_damage tracknums = DeriveTest.modify_constant $ \st ->
    st { Derive.state_score_damage = mkdamage tracknums }

mkdamage :: [TrackNum] -> Derive.ScoreDamage
mkdamage tracknums = mempty
    { Derive.sdamage_tracks =
        Map.fromList [(UiTest.mk_tid n, Ranges.range 0 1) | n <- tracknums]
    }
