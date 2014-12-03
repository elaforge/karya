-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Legong_test where
import qualified Data.List as List

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Global


test_note_to_call = do
    let run key ps = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks ("scale=" ++ key) $
            UiTest.note_track [(t, 1, p) | (t, p) <- zip (Seq.range_ 0 1) ps]
        from_ding = drop (7+5) Legong.umbang

    -- Default to umbang.
    equal (run "legong" ["1i"])
        ([Just $ head (Legong.extend Legong.umbang)], [])
    equal (run "legong" ["tuning=isep | -- 1i"])
        ([Just $ head (Legong.extend Legong.isep)], [])
    equal (run "legong" ["4i", "4o", "4e", "4e#"])
        (map Just (take 4 from_ding), [])

    -- baro is 1 345 7
    equal (run "legong | key=baro"
            ["4i", "4i#", "4o", "4e", "4u", "4u#", "4a", "5i"])
        (map Just (take 8 from_ding), [])

    equal (run "legong-pemade" ["i-", "o-", "e-"])
        (map Just (take 3 from_ding), [])
    equal (run "legong-pemade" ["i-"]) ([Just 72.46], [])

test_input_to_note = do
    let f scale key = either prettyt Pitch.note_text
            . Scale.scale_input_to_note scale
                (maybe mempty Scales.key_environ key)
            . CmdTest.ascii_kbd . (\(a, b, c) -> CmdTest.pitch a b c)
        legong = lookup_scale "legong"
        legong_p = lookup_scale "legong-pemade"
        invalid = "invalid input"
    -- baro is 1 345 7
    --         i oeu a
    equal (map (f legong (Just "baro"))
            [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]])
        [ "4i", "4i#", "4o", invalid, "4e", invalid, "4u", "4u#", "4a", invalid
        , "5i", "5i#"
        ]
    equal (f legong (Just "sunaren") (4, 0, 0)) "4i"
    equal (map (f legong_p (Just "baro")) $
            take 12 [(oct, pc, 0) | oct <- [3, 4, 5], pc <- [0..4]])
        [ invalid, "o_", "e_", "u_", "a_", "i-", "o-", "e-", "u-", "a-"
        , "i^", invalid
        ]

lookup_scale :: Pitch.ScaleId -> Scale.Scale
lookup_scale scale_id = fromMaybe (error $ "no scale: " <> show scale_id) $
    List.find ((==scale_id) . Scale.scale_id) Legong.scales
