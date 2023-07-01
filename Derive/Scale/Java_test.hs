-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Java_test where
import qualified Util.Debug as Debug
import qualified Util.Lists as Lists
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Java as Java
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_read :: Test
test_read = do
    -- let f scale key pitch = read_scale scale key pitch
    let f = read_scale
    equal (f pelog lima "51") (Right "5-0")
    equal (f panerus lima "1") (Right "5-0")
    equal (f panerus lima "2") (Right "5-1")
    equal (f panerus lima "`1^`") (Right "6-0")
    -- Notation is absolute, even if input is relative to key.
    equal (f panerus barang "1") (Right "5-0")

    -- equal (f "wayang-pemade" "i^") (Right "5-0")
    -- equal (f "wayang-kantilan" "i-") (Right "5-0")
    -- let run scale pitch = DeriveTest.extract Score.initial_nn $
    --         DeriveTest.derive_tracks "" $ scale_track scale [pitch]
    -- equal (run "wayang" "5i") (run "wayang-pemade" "i^")
    -- equal (run "wayang" "6i") (run "wayang-kantilan" "i^")

test_input_to_note :: Test
test_input_to_note = do
    let f scale key = ScaleTest.input_to_note scale (ScaleTest.key_environ key)
        invalid = "invalid input"
    equal (f pelog lima (5, 0, 0)) "51"
    -- equal (f pelog lima (5, 1, 0)) "52"
    -- equal (f pelog lima (5, 2, 0)) "53"
    equal (f pelog lima (5, 3, 0)) "55"

    -- equal (f panerus (5, 0, 0)) "1"
    -- equal (f panerus (6, 0, 0)) "`1^`"
    -- equal [f panerus (5, pc, 0) | pc <- [0..5]] ["1", "2", "3", "5", "6"]

    -- equal (f wayang_pemade (3, 1, 0)) "o_"
    -- equal (f wayang_pemade (4, 0, 0)) "i-"
    -- equal (map (f pelog) [(4, pc, 0) | pc <- [0..5]])
    --     ["4i", "4o", "4e", "4u", "4a", "5i"]
    -- equal (map (f wayang_pemade) $
    --         take 12 [(oct, pc, 0) | oct <- [3..6], pc <- [0..4]])
    --     [ invalid, "o_", "e_", "u_", "a_", "i-", "o-", "e-", "u-", "a-"
    --     , "i^", invalid
    --     ]

test_input_to_nn :: Test
test_input_to_nn = do
    let run scale = DeriveTest.eval Ui.empty . Scale.scale_input_to_nn scale 0
            . ScaleTest.ascii_kbd
    equal (run pelog (5, 0, 0)) (Right (Right 74.25))
    equal (run pelog (5, 1, 0)) (Right (Right 75.68))
    equal (run panerus (3, 4, 0)) (Right (Left DeriveT.InvalidInput))
    equal (run panerus (3, 5, 0)) (Right (Right 58.68))
    equal (run panerus (4, 0, 0)) (Right (Right 62.18))
    equal (run panerus (5, 1, 0)) (Right (Right 75.68))

lima, barang :: Text
lima = "lima"
barang = "barang"

pelog :: Scale.Scale
pelog = get_scale "pelog"

panerus :: Scale.Scale
panerus = get_scale "pelog-gender-panerus"

get_scale :: Text -> Scale.Scale
get_scale = ScaleTest.get_scale Java.scales

-- * util
--
-- TODO copy paste from Wayang_test

read_scale :: Scale.Scale -> Text -> Pitch.Note -> Either Text Text
read_scale scale key note = bimap pretty pretty $
    -- Scale.scale_read scale (ScaleTest.key_environ key) note
    Scale.scale_read scale mempty note

scale_track :: Text -> [Text] -> [UiTest.TrackSpec]
scale_track scale_id pitches =
    [ (">", [(n, 1, "") | n <- map fst events])
    , ("*" <> scale_id, [(n, 0, p) | (n, p) <- events])
    ]
    where events = zip (Lists.range_ 0 1) pitches
