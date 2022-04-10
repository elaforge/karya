-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.BaliScales_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import Global


test_ombak :: Test
test_ombak = do
    let run trans pitch = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks ""
                [("> " <> trans, [(0, 1, "")]), ("*wayang", [(0, 0, pitch)])]
    equal (run "" "4e") ([Just 67.29], [])
    equal (run "| tuning = umbang" "4e") ([Just 67.29], [])
    equal (run "| tuning = isep" "4e") ([Just 67.6], [])

    let both ombak =
            ( run ("| tuning=umbang | %ombak=" <> showt ombak) "4e"
            , run ("| tuning=isep | %ombak=" <> showt ombak) "4e"
            )
    equalf 0.01 (both 5)  (([Just 67.338], []), ([Just 67.553], []))
    equalf 0.01 (both 10) (([Just 67.229], []), ([Just 67.66], []))
