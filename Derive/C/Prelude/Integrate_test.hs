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
    let res = DeriveTest.deriveTracksSetup (withDamage [2]) ""
            [ ("c1", [(0, 0, ".5")])
            , ("c2", [(0, 0, ".75")])
            , (">i2 | < (list c2)", [(0, 1, "")])
            , ("*", [(0, 0, "4c")])
            , (">i2", [(0, 2, "")]) -- appears in non-integrated output
            ]
        extract i = (Derive.integratedSource i, Derive.integratedEvents i)
    let [(source, integrated)] = map extract (Derive.rIntegrated res)
    equal source (Right (UiTest.mkTid 3))
    let eNote e =
            ( DeriveTest.eNote e
            , filter ((/= Controls.dynamicIntegrate) . fst) $
                DeriveTest.eControls e
            )
    equal (DeriveTest.extractLevents eNote (Stream.toList integrated))
        ( [((0, 1, "4c"), [("c2", [(0, 0.75)])])]
        , []
        )
    equal (DeriveTest.extract
            (\e -> (DeriveTest.eNote e, DeriveTest.eInstrument e)) res)
        ([((0, 2, "?"), "i2")], [])

withDamage :: [TrackNum] -> DeriveTest.Setup
withDamage tracknums = DeriveTest.modifyConstant $ \st ->
    st { Derive.stateScoreDamage = mkdamage tracknums }

mkdamage :: [TrackNum] -> Derive.ScoreDamage
mkdamage tracknums = mempty
    { Derive.sdamageTracks =
        Map.fromList [(UiTest.mkTid n, Ranges.range 0 1) | n <- tracknums]
    }
