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
    let f laras = ScaleTest.note_to_call "selisir" ("laras=" <> laras)
    let nns n = take n . from_4i True
    equal (f "rambat" ["4i", "4o", "4e", "4u"])
        (map Just $ nns 4 Selisir.laras_rambat, [])
    -- Selisir is a true 5 note scale, and shouldn't accept pemero.
    -- TODO fixing this will require a BaliScales overhaul
    -- equal (f "rambat" ["4e#"]) ([Nothing], [])
    let Just teges = Map.lookup "pegulingan-teges" Selisir.laras
    equal (f "pegulingan-teges" ["4i", "4o", "4e", "4u", "4a", "5i"])
        (map Just $ nns 6 teges, [])
    equal (f "gong-peliatan" ["4i", "4o"])
        (map (Just . Pitch.hz_to_nn) [280, 305], [])

test_input_to_note = do
    let input_to_note = ScaleTest.input_to_note selisir mempty
        invalid = "invalid input"
    equal (map (input_to_note) [(4, pc, acc) | pc <- [0, 1, 2], acc <- [0, 1]])
        ["4i", invalid, "4o", invalid, "4e", invalid]

from_4i :: Bool -> BaliScales.Laras -> [Pitch.NoteNumber]
from_4i umbang laras = drop 5 $ Vector.toList (nns laras)
    where
    nns = if umbang then BaliScales.laras_umbang else BaliScales.laras_isep

selisir :: Scale.Scale
selisir = ScaleTest.get_scale Selisir.scales "selisir"
