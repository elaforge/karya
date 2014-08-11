-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Legong_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch


test_note_to_call = do
    let run key ps = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks ("scale=legong" ++ key) $
            UiTest.note_track [(t, 1, p) | (t, p) <- zip (Seq.range_ 0 1) ps]
    -- Default to umbang.
    equal (run "" ["1i"]) ([Just $ head Legong.umbang], [])
    equal (run "" ["tuning=isep | -- 1i"]) ([Just $ head Legong.isep], [])
    let from_ding = drop (2*7) Legong.umbang
    equal (run "" ["3i", "3o", "3e", "3e#"]) (map Just (take 4 from_ding), [])

    -- baro is 1 345 7
    equal (run " | key=baro" ["3i", "3i#", "3o", "3e", "3u", "3u#", "3a", "4i"])
        (map Just (take 8 from_ding), [])

test_input_to_note = do
    let f key = either prettyt Pitch.note_text
            . Scale.scale_input_to_note (head Legong.scales)
                (Just (Pitch.Key key))
            . ascii
        ascii (oct, pc, accs) =
            Pitch.Input Pitch.AsciiKbd (CmdTest.pitch oct pc accs) 0
    equal (map (f "baro") [(4, 0, 0), (4, 1, 0), (4, 2, 0)])
        ["4i", "4o", "4e"]
    equal (f "sunaren" (4, 0, 0)) "4i"
