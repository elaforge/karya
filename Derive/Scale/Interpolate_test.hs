-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Interpolate_test where
import qualified Util.Lists as Lists
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import Global


test_interpolate :: Test
test_interpolate = do
    let run scaleAt = DeriveTest.extract extract $ DeriveTest.deriveTracks
            "scale-from=twelve-r | scale-to=raga | key-to=hemavati\
                \ | scale=interpolate" $
            ("scale-at", scaleAt)
                : UiTest.noteTrack [(n, 1, p) | (n, p) <- notes]
            where
            notes = zip (Lists.range_ 0 1) ["4s", "4r", "4g", "4m"]
        extract e = fromMaybe (error "no nn") $ Score.initialNn e
    let major = [60, 62, 64, 65]
        hemavati = [Pitch.modifyHz (*n) 60 | n <- [1, 9/8, 6/5, 7/5]]
    equal (run [(0, 0,"0")]) (major, [])
    equalf 0.01 (run [(0, 0,"1")]) (hemavati, [])
    equalf 0.01 (run [(0, 0, ".5")]) (map (/2) (zipWith (+) major hemavati), [])
