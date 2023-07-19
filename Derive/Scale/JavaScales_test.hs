-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JavaScales_test where
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Scale.JavaScales as JavaScales
import qualified Perform.Pitch as Pitch

import           Util.Affine
import           Util.Test


test_add_diatonic :: Test
test_add_diatonic = do
    let f c = JavaScales.add_diatonic lima (Chromatic c)
    -- 51 52 53 55 56 61 62 63
    equal (map (f 0) [0..7]) $ map Chromatic [0, 1, 2, 4, 5, 7, 8, 9]
    -- 52 53 55 56 61 62 63 65
    equal (map (f 1) [0..7]) $ map Chromatic [1, 2, 4, 5, 7, 8, 9, 11]
    equal (f 3 0) (Chromatic 3) -- 54+0 = 54
    equal (f 3 1) (Chromatic 4) -- 54+1 = 55
    equal (f 3 (-1)) (Chromatic 2) -- 54-1 = 53
    equal (map (f 7) [0, -1 .. -7]) $ map Chromatic [7, 5, 4, 2, 1, 0, -2, -3]
    equal (map (f 7) [0, -1 .. -7]) $ map Chromatic [7, 5, 4, 2, 1, 0, -2, -3]

test_read_pitch_absolute :: Test
test_read_pitch_absolute = do
    let f = JavaScales.read_pitch_absolute
    equal (f lima "40") (Left $ DeriveT.UnparseableNote "40")
    equal (f lima "48") (Left $ DeriveT.UnparseableNote "48")
    let notes = ["41", "42", "43", "44", "45", "46", "47", "51"]
    equal (map (f lima) notes) $ map (Right . pitch)
        [ (4, 0, 0), (4, 1, 0), (4, 2, 0), (4, 2, 1), (4, 3, 0), (4, 4, 0)
        , (4, 4, 1), (5, 0, 0)
        ]
    equal (map (f barang) notes) $ map (Right . pitch)
        [ (3, 4, 1), (4, 0, 0), (4, 1, 0), (4, 1, 1), (4, 2, 0), (4, 3, 0)
        , (4, 4, 0), (4, 4, 1)
        ]
    right_equal (f barang "41") $ pitch (3, 4, 1)
    right_equal (f barang "42") $ pitch (4, 0, 0)

test_show_pitch_absolute :: Test
test_show_pitch_absolute = do
    let f layout = JavaScales.show_pitch_absolute layout . pitch
    let err = Left (DeriveT.PitchError "degree not in scale")
    equal (map (f lima) [(4, pc, 0) | pc <- [0..5]]) $
        map Right ["41", "42", "43", "45", "46"] ++ [err]
    equal (map (f barang) [(4, pc, 0) | pc <- [0..5]]) $
        map Right ["42", "43", "45", "46", "47"] ++ [err]
    right_equal (f barang (3, 4, 1)) "41"
    right_equal (f barang (4, 0, 0)) "42"

    -- right_equal (f lima (4, 0, 0)) "41"
    -- right_equal (f lima (4, 1, 0)) "42"
    -- right_equal (f lima (4, 2, 0)) "43"
    -- right_equal (f barang (4, 0, 0)) "41"

pitch :: (Pitch.Octave, Pitch.PitchClass, Pitch.Accidentals) -> Pitch.Pitch
pitch (oct, pc, acc) = Pitch.Pitch oct (Pitch.Degree pc acc)

-- t0 = map (read_pitch_absolute2 barang) ["51", "52"]

lima :: JavaScales.Layout
lima = JavaScales.make_layout 0 [1, 1, 2, 1, 2]

barang :: JavaScales.Layout
barang = JavaScales.make_layout 1 [1, 2, 1, 1, 2] -- 23567
