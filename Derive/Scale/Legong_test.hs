-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Legong_test where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Util.Test
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Scale.McPhee as McPhee
import qualified Derive.Scale.ScaleTest as ScaleTest

import qualified Perform.Pitch as Pitch
import Global


test_note_to_call = do
    let run scale title = ScaleTest.note_to_call scale title
        ding = from_4i True Legong.saih_rambat

    -- 4i is close to middle C (60nn).
    equal (run "legong" "" ["4i"]) ([Just (head ding)], [])
    equal (run "legong" "" ["5i"]) ([Just (ding !! 7)], [])
    -- *legong-c is centered around pemade middle ding
    let i5 = drop 7 ding
    equal (run "legong-c" "" ["1"]) ([Just (head i5)], [])
    equal (run "legong-pemade" "" ["i-"]) ([Just (head i5)], [])
    equal (run "legong-pemade" "" ["i-", "o-", "e-"]) (map Just (take 3 i5), [])

    equal (run "legong" "" ["tuning=isep | -- 4i"])
        ([Just (head (from_4i False Legong.saih_rambat))], [])
    equal (run "legong" "" ["4i", "4o", "4e", "4e#"])
        (map Just (take 4 ding), [])

    equal (run "legong | key=baro" ""
            ["4i", "4o", "4e", "4e#", "4u", "4a", "4a#", "5i"])
        (map Just (take 8 (drop 2 ding)), [])

    -- Baro ding == selisir's deng.
    equal (run "legong | key=baro" "" ["4i"])
        (run "legong | key=selisir" "" ["4e"])

test_note_to_call_mcphee = do
    let run saih key = ScaleTest.note_to_call "legong"
            ("saih=" <> saih <> " | " <> "key=" <> key)
    -- Ensure that when I convert tembung to selisir, I get ding in
    -- the correct place according to McPhee.
    equal (run "gambuh-tabanan" "selisir" ["4i", "4o", "4e"])
        (map (Just . Pitch.hz_to_nn) [280, 303, 325], [])

Just saih = Map.lookup "gambuh-tabanan" Legong.saihs
saih_nns = map Pitch.hz_to_nn $ McPhee.hz $ McPhee.saih_pitu !! 4

selisir_nns = Vector.toList $ BaliScales.saih_umbang saih
selisir_hz = map Pitch.nn_to_hz selisir_nns

test_input_to_note = do
    let f scale key = ScaleTest.input_to_note scale
            (maybe mempty ScaleTest.key_environ key)
        invalid = "invalid input"

    -- Ding is relative, so the input pattern is always the same.
    let pattern =
            [ "4i", invalid, "4o", invalid, "4e", "4e#", "4u", invalid
            , "4a", "4a#", "5i", invalid
            ]
        keys = [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]]
    equal (map (f legong (Just "selisir")) keys) pattern
    equal (map (f legong (Just "baro")) keys) pattern
    equal (map (f legong (Just "tembung")) keys) pattern

    let legong_pemade = ScaleTest.get_scale Legong.scales "legong-pemade"
    equal (map (f legong_pemade Nothing) $
            take 12 [(oct, pc, 0) | oct <- [4..6], pc <- [0..4]])
        [ invalid, "o_", "e_", "u_", "a_", "i-", "o-", "e-", "u-", "a-"
        , "i^", invalid
        ]

legong :: Scale.Scale
legong = ScaleTest.get_scale Legong.scales "legong"

from_4i :: Bool -> BaliScales.Saih -> [Pitch.NoteNumber]
from_4i umbang saih = drop (2 + 5) $ Vector.toList (nns saih)
    where nns = if umbang then BaliScales.saih_umbang else BaliScales.saih_isep
    -- 2 to undo 'Legong.extend', 5 to get to ding
