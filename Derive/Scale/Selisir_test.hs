-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Selisir_test where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Util.Test
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Scale.Selisir as Selisir

import qualified Perform.Pitch as Pitch
import Global



test_note_to_call = do
    let f saih = ScaleTest.note_to_call "selisir" ("saih=" <> saih)
    let nns n = take n . from_4i True
    equal (f "rambat" ["4i", "4o", "4e", "4e#"])
        (map Just $ nns 4 Selisir.saih_rambat, [])

    let Just teges = Map.lookup "pegulingan-teges" Selisir.saihs
    equal (f "pegulingan-teges" ["4i", "4o", "4e", "4u", "4a", "5i"])
        (map Just $ nns 6 teges, [])
    equal (f "pegulingan-teges" ["4e#"]) ([Nothing], ["error"])

test_input_to_note = do
    let input_to_note = ScaleTest.input_to_note selisir mempty
        invalid = "invalid input"
    equal (map (input_to_note) [(4, pc, acc) | pc <- [0, 1, 2], acc <- [0, 1]])
        ["4i", invalid, "4o", invalid, "4e", invalid]

from_4i :: Bool -> BaliScales.Saih -> [Pitch.NoteNumber]
from_4i umbang saih = drop 5 $ Vector.toList (nns saih)
    where nns = if umbang then BaliScales.saih_umbang else BaliScales.saih_isep

selisir :: Scale.Scale
selisir = ScaleTest.get_scale Selisir.scales "selisir"
