-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Legong_test where
import qualified Data.Vector as Vector

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Global


test_note_to_call = do
    let run key ps = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks ("scale=" ++ key) $
            UiTest.note_track [(t, 1, p) | (t, p) <- zip (Seq.range_ 0 1) ps]
        umbang = Vector.toList $ BaliScales.saih_umbang Legong.saih_rambat
        isep = Vector.toList $ BaliScales.saih_isep Legong.saih_rambat
        -- 2 to undo 'Legong.extend', 5 to get to ding
        ding = drop (2 + 5) umbang

    -- 4i is close to middle C (60nn).
    equal (run "legong" ["4i"]) ([Just (head ding)], [])
    equal (run "legong" ["3i"])
        ([Just $ head umbang], [])
    -- *legong-c is centered around pemade middle ding
    let i5 = drop 7 ding
    equal (run "legong-c" ["1"]) ([Just (head i5)], [])
    equal (run "legong-pemade" ["i-"]) ([Just (head i5)], [])
    equal (run "legong-pemade" ["i-", "o-", "e-"]) (map Just (take 3 i5), [])

    equal (run "legong" ["tuning=isep | -- 3i"])
        ([Just $ head isep], [])
    equal (run "legong" ["4i", "4o", "4e", "4e#"])
        (map Just (take 4 ding), [])

    -- baro is 1 345 7
    equal (run "legong | key=baro"
            ["4i", "4i#", "4o", "4e", "4u", "4u#", "4a", "5i"])
        (map Just (take 8 ding), [])

test_input_to_note = do
    let f scale key = either pretty Pitch.note_text
            . Scale.scale_input_to_note scale
                (maybe mempty ScaleTest.key_environ key)
            . CmdTest.ascii_kbd . (\(a, b, c) -> CmdTest.pitch a b c)
        legong = ScaleTest.get_scale Legong.scales "legong"
        legong_pemade = ScaleTest.get_scale Legong.scales "legong-pemade"
        invalid = "invalid input"

    -- baro is 1 345 7
    --         i oeu a
    equal (map (f legong (Just "baro"))
            [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]])
        [ "4i", "4i#", "4o", invalid, "4e", invalid, "4u", "4u#", "4a", invalid
        , "5i", "5i#"
        ]
    equal (f legong (Just "sunaren") (4, 0, 0)) "4i"
    equal (map (f legong_pemade (Just "baro")) $
            take 12 [(oct, pc, 0) | oct <- [4..6], pc <- [0..4]])
        [ invalid, "o_", "e_", "u_", "a_", "i-", "o-", "e-", "u-", "a-"
        , "i^", invalid
        ]
