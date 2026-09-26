-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Selisir_test where
import qualified Data.Map as Map
import qualified Data.Vector as Vector

import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Scale.Selisir as Selisir

import qualified Perform.Pitch as Pitch

import           Util.Test


test_note_to_call :: Test
test_note_to_call = do
    let f laras = ScaleTest.noteToCall "selisir" ("laras=" <> laras)
    let nns n = take n . from4i True
    equal (f "rambat" ["4i", "4o", "4e", "4u"])
        (map Just $ nns 4 Selisir.larasRambat, [])
    -- Selisir is a true 5 note scale, and shouldn't accept pemero.
    -- TODO fixing this will require a BaliScales overhaul
    -- equal (f "rambat" ["4e#"]) ([Nothing], [])
    let Just teges = Map.lookup "pegulingan-teges" Selisir.laras
    equal (f "pegulingan-teges" ["4i", "4o", "4e", "4u", "4a", "5i"])
        (map Just $ nns 6 teges, [])
    equal (f "gong-peliatan" ["4i", "4o"])
        (map (Just . Pitch.hzToNn) [280, 305], [])

test_input_to_note :: Test
test_input_to_note = do
    let inputToNote = ScaleTest.inputToNote selisir mempty
        invalid = "invalid input"
    equal (map (inputToNote) [(4, pc, acc) | pc <- [0, 1, 2], acc <- [0, 1]])
        ["4i", invalid, "4o", invalid, "4e", invalid]

from4i :: Bool -> BaliScales.Laras -> [Pitch.NoteNumber]
from4i umbang laras = drop 5 $ Vector.toList (nns laras)
    where
    nns = if umbang then BaliScales.larasUmbang else BaliScales.larasIsep

selisir :: Scale.Scale
selisir = ScaleTest.getScale Selisir.scales "selisir"
