-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Java_test where
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Java as Java
import qualified Derive.Scale.JavaScales as JavaScales
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_read :: Test
test_read = do
    let f scale = fmap pretty . read_scale scale
    let unparseable n = Left $ "unparseable note: " <> n
    equal (f pelog_lima "40") (unparseable "40")
    equal (f pelog_lima "41") (Right "4-0")
    equal (f pelog_lima "48") (unparseable "48")
    equal (f panerus_lima "0") (unparseable "0")
    equal (f panerus_lima "8") (unparseable "8")
    --      1      2      3       4      5      6       7   1^
    equal (map (f pelog_lima . note4) [1..7]) $ map Right
        ["4-0", "4-1", "4-2", "4-2#", "4-3", "4-4", "4-4#"]
    equal (map (f panerus_lima . Pitch.Note . showt) [1..7]) $ map Right
        ["4-0", "4-1", "4-2", "4-2#", "4-3", "4-4", "4-4#"]
    equal (map (f pelog_barang . note4) [1..7]) $ map Right
        ["3-4#", "4-0", "4-1", "4-1#", "4-2", "4-3", "4-4"]
    equal (map (f panerus_barang . Pitch.Note . showt) [1..7]) $ map Right
        ["3-4#", "4-0", "4-1", "4-1#", "4-2", "4-3", "4-4"]

test_show :: Test
test_show = do
    let f scale note = (show_scale scale =<< read_scale scale note, pretty note)
    mapM_ (uncurry right_equal) (map (f pelog_lima . note4) [1..7])
    mapM_ (uncurry right_equal) (map (f pelog_barang . note4) [1..7])
    mapM_ (uncurry right_equal) (map (f panerus_lima . Pitch.Note . showt)
        [1..7])
    mapM_ (uncurry right_equal) (map (f panerus_barang . Pitch.Note . showt)
        [1..7])

test_transpose :: Test
test_transpose = do
    let f scale t steps note =
            Scale.scale_show scale mempty
            =<< Scale.scale_transpose scale t mempty steps
            =<< Scale.scale_read scale mempty note
    right_equal (f pelog_lima Derive.Diatonic 1 "43") "45"
    right_equal (f pelog_lima Derive.Diatonic 2 "43") "46"
    right_equal (f pelog_lima Derive.Diatonic 3 "43") "51"
    right_equal (f pelog_lima Derive.Diatonic 1 "44") "45"
    right_equal (f pelog_lima Derive.Chromatic 1 "43") "44"

    equal (map (f pelog_lima Derive.Chromatic 1 . note4) [1..7]) $
        map Right ["42", "43", "44", "45", "46", "47", "51"]
    equal (map (f pelog_lima Derive.Diatonic 1 . note4) [1..7]) $
        map Right ["42", "43", "45", "45", "46", "51", "51"]

    right_equal (f pelog_barang Derive.Diatonic 0 "47") "47"
    right_equal (f pelog_barang Derive.Diatonic 1 "47") "52"
    right_equal (f pelog_barang Derive.Diatonic 1 "51") "52"
    equal (map (f pelog_barang Derive.Chromatic 1 . note4) [1..7]) $
        map Right ["42", "43", "44", "45", "46", "47", "51"]
    equal (map (f pelog_barang Derive.Diatonic 1 . note4) [1..7]) $
        map Right ["42", "43", "45", "45", "46", "47", "52"]

test_input_to_note :: Test
test_input_to_note = do
    let f scale = ScaleTest.input_to_note scale mempty
        x = "invalid input"
    equal (f pelog_lima (4, 0, 0)) "41"
    equal (f pelog_lima (4, 1, 0)) "42"
    equal (f pelog_lima (4, 2, 0)) "43"
    equal (f pelog_lima (4, 2, 1)) "44"
    equal (f pelog_lima (4, 3, 0)) "45"
    equal (f pelog_lima (4, 4, 0)) "46"
    equal (f pelog_lima (4, 5, 0)) "51"

    equal (map (f pelog_lima) [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]])
        [ "41", x
        , "42", x
        , "43", "44"
        , "45", x
        , "46", "47"
        , "51", x
        ]

    equal (f pelog_barang (4, 4, 0)) "47"
    equal (f pelog_barang (4, 4, 1)) "51"
    -- 23567
    equal (map (f pelog_barang) [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]])
        [ "42", x
        , "43", "44"
        , "45", x
        , "46", x
        , "47", "51"
        , "52", x
        ]

    equal (f panerus_lima (4, 0, 0)) "1"
    equal (f panerus_lima (4, 1, 0)) "2"

    equal (f panerus_lima (5, 0, 0)) "`1^`"
    equal (f panerus_lima (5, 2, 0)) "`3^`"
    equal (f panerus_lima (5, 3, 0)) x
    equal (f panerus_lima (3, 4, 0)) "`6.`"
    equal (f panerus_lima (2, 4, 0)) "`6..`"
    equal (f panerus_lima (2, 3, 0)) x

test_input_to_nn :: Test
test_input_to_nn = do
    let run scale = DeriveTest.eval Ui.empty . Scale.scale_input_to_nn scale 0
            . ScaleTest.ascii_kbd
    equal (run pelog_lima (4, 0, 0)) (Right (Right 74.25))
    equal (run pelog_lima (4, 1, 0)) (Right (Right 75.68))
    equal (run panerus_lima (2, 3, 0)) (Right (Left DeriveT.InvalidInput))
    equal (run panerus_lima (2, 4, 0)) (Right (Right 58.68))
    equal (run panerus_lima (3, 0, 0)) (Right (Right 62.18))
    equal (run panerus_lima (4, 1, 0)) (Right (Right 75.68))

test_note_call :: Test
test_note_call = do
    let derive scale control =
            DeriveTest.extract extract . derive_pitch scale "" control
        extract e = (Score.initial_nn e, Score.initial_note e)
    let f = derive "pelog-lima"
    equal (f "" "41") ([(Just nn41, Just "41")], [])
    equal (f "t-chrom=1" "41") ([(Just nn42, Just "42")], [])
    equal (f "t-chrom=3" "41") ([(Just 80, Just "44")], [])
    equal (f "t-dia=1" "41") ([(Just nn42, Just "42")], [])
    -- symbolic pitch rounds to 54, but it's actually a bit higher.
    equal (f "t-dia=2.6" "41") ([(Just 80.206, Just "44")], [])
    equal (f "t-dia=3" "41") ([(Just 81.03, Just "45")], [])
    equal (f "t-dia=-1" "41") ([(Just 70.5, Just "36")], [])
    equal (f "t-oct=1" "41") ([(Just 86.4, Just "51")], [])
    equal (f "t-nn=1" "41") ([(Just (nn41 + 1), Just "41")], [])
    equal (f "t-hz=7" "41")
        ([(Just (Pitch.modify_hz (+7) nn41), Just "41")], [])

    let gender = derive "pelog-barang-gender-panerus" ""
    let absolute = derive "pelog-barang" ""
    equal (absolute "41") ([(Just nn41, Just "41")], [])
    equal (gender "1") ([(Just nn41, Just "1")], [])
    equal (f "" "42") ([(Just nn42, Just "42")], [])
    equal (gender "2") ([(Just nn42, Just "2")], [])

nn41 :: Pitch.NoteNumber
nn41 = 74.25

nn42 :: Pitch.NoteNumber
nn42 = 75.68

note4 :: Int -> Pitch.Note
note4 = Pitch.Note . ("4"<>) . showt

lima, barang :: Text
lima = "lima"
barang = "barang"

pelog :: Scale.Scale
pelog = get_scale "pelog"

pelog_lima :: Scale.Scale
pelog_lima = get_scale "pelog-lima"

pelog_barang :: Scale.Scale
pelog_barang = get_scale "pelog-barang"

panerus_lima :: Scale.Scale
panerus_lima = get_scale "pelog-lima-gender-panerus"

panerus_barang :: Scale.Scale
panerus_barang = get_scale "pelog-barang-gender-panerus"

get_scale :: Text -> Scale.Scale
get_scale = ScaleTest.get_scale Java.scales

-- * util
--
-- TODO copy paste from Wayang_test

show_scale :: Scale.Scale -> Pitch.Pitch -> Either Text Text
show_scale scale = bimap pretty pretty . Scale.scale_show scale mempty

read_scale :: Scale.Scale -> Pitch.Note -> Either Text Pitch.Pitch
read_scale scale = first pretty . Scale.scale_read scale mempty

scale_track :: Text -> [Text] -> [UiTest.TrackSpec]
scale_track scale_id pitches =
    [ (">", [(n, 1, "") | n <- map fst events])
    , ("*" <> scale_id, [(n, 0, p) | (n, p) <- events])
    ]
    where events = zip (Lists.range_ 0 1) pitches

-- TODO from Twelve_test
derive_pitch :: Text -> Text -> Text-> Text -> Derive.Result
derive_pitch scale key control pitch =
    DeriveTest.derive_tracks (if key == "" then "" else "key = " <> key)
        [ (Texts.join2 " | " ">" control, [(0, 1, "")])
        , ("*" <> scale, [(0, 0, pitch)])
        ]
