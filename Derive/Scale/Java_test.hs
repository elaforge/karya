-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Java_test where
import qualified Util.Lists as Lists
import qualified Util.Texts as Texts
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_read :: Test
test_read = do
    let f scale = fmap pretty . readScale scale
    let unparseable n = Left $ "unparseable note: " <> n
    equal (f pelogLima "40") (unparseable "40")
    equal (f pelogLima "41") (Right "4-0")
    equal (f pelogLima "48") (unparseable "48")
    equal (f panerusLima "0") (unparseable "0")
    equal (f panerusLima "8") (unparseable "8")
    --      1      2      3       4      5      6       7   1^
    equal (map (f pelogLima . note4) [1..7]) $ map Right
        ["4-0", "4-1", "4-2", "4-2#", "4-3", "4-4", "4-4#"]
    equal (map (f panerusLima . Pitch.Note . showt) [1..7]) $ map Right
        ["4-0", "4-1", "4-2", "4-2#", "4-3", "4-4", "4-4#"]
    equal (map (f pelogBarang . note4) [1..7]) $ map Right
        ["3-4#", "4-0", "4-1", "4-1#", "4-2", "4-3", "4-4"]
    equal (map (f panerusBarang . Pitch.Note . showt) [1..7]) $ map Right
        ["3-4#", "4-0", "4-1", "4-1#", "4-2", "4-3", "4-4"]

test_show :: Test
test_show = do
    let f scale note = (showScale scale =<< readScale scale note, pretty note)
    mapM_ (uncurry rightEqual) (map (f pelogLima . note4) [1..7])
    mapM_ (uncurry rightEqual) (map (f pelogBarang . note4) [1..7])
    mapM_ (uncurry rightEqual) (map (f panerusLima . Pitch.Note . showt)
        [1..7])
    mapM_ (uncurry rightEqual) (map (f panerusBarang . Pitch.Note . showt)
        [1..7])

test_transpose :: Test
test_transpose = do
    let f scale t steps note =
            Scale.scaleShow scale mempty
            =<< Scale.scaleTranspose scale t mempty steps
            =<< Scale.scaleRead scale mempty note
    rightEqual (f pelogLima Derive.Diatonic 1 "43") "45"
    rightEqual (f pelogLima Derive.Diatonic 2 "43") "46"
    rightEqual (f pelogLima Derive.Diatonic 3 "43") "51"
    rightEqual (f pelogLima Derive.Diatonic 1 "44") "45"
    rightEqual (f pelogLima Derive.Chromatic 1 "43") "44"

    equal (map (f pelogLima Derive.Chromatic 1 . note4) [1..7]) $
        map Right ["42", "43", "44", "45", "46", "47", "51"]
    equal (map (f pelogLima Derive.Diatonic 1 . note4) [1..7]) $
        map Right ["42", "43", "45", "45", "46", "51", "51"]

    rightEqual (f pelogBarang Derive.Diatonic 0 "47") "47"
    rightEqual (f pelogBarang Derive.Diatonic 1 "47") "52"
    rightEqual (f pelogBarang Derive.Diatonic 1 "51") "52"
    equal (map (f pelogBarang Derive.Chromatic 1 . note4) [1..7]) $
        map Right ["42", "43", "44", "45", "46", "47", "51"]
    equal (map (f pelogBarang Derive.Diatonic 1 . note4) [1..7]) $
        map Right ["42", "43", "45", "45", "46", "47", "52"]

test_input_to_note :: Test
test_input_to_note = do
    let f scale = ScaleTest.inputToNote scale mempty
        x = "invalid input"
    equal (f pelogLima (4, 0, 0)) "41"
    equal (f pelogLima (4, 1, 0)) "42"
    equal (f pelogLima (4, 2, 0)) "43"
    equal (f pelogLima (4, 2, 1)) "44"
    equal (f pelogLima (4, 3, 0)) "45"
    equal (f pelogLima (4, 4, 0)) "46"
    equal (f pelogLima (4, 5, 0)) "51"
    equal (map (f pelogLima) [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]])
        [ "41", x
        , "42", x
        , "43", "44"
        , "45", x
        , "46", "47"
        , "51", x
        ]

    equal (f pelogBarang (4, 4, 0)) "47"
    equal (f pelogBarang (4, 4, 1)) "51"
    -- 23567
    equal (map (f pelogBarang) [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]])
        [ "42", x
        , "43", "44"
        , "45", x
        , "46", x
        , "47", "51"
        , "52", x
        ]
    equal (f panerusLima (4, 0, 0)) "1"
    equal (f panerusLima (4, 1, 0)) "2"
    equal (f panerusLima (5, 0, 0)) "`1^`"
    equal (f panerusLima (5, 2, 0)) "`3^`"
    match (f panerusLima (5, 3, 0)) "out of instrument range"
    equal (f panerusLima (3, 4, 0)) "`6.`"
    equal (f panerusLima (2, 4, 0)) "`6..`"
    match (f panerusLima (2, 3, 0)) "out of instrument range"

test_input_to_nn :: Test
test_input_to_nn = do
    let run scale = either (Left . pretty) (either (Left . pretty) Right)
            . DeriveTest.eval Ui.empty
            . Scale.scaleInputToNn scale 0 . ScaleTest.asciiKbd
    pprint (run pelogLima (4, 0, 0))
    rightEqual (run pelogLima (4, 0, 0)) nn41
    rightEqual (run pelogLima (4, 1, 0)) nn42
    leftLike (run panerusLima (2, 3, 0)) "out of instrument range"
    rightEqual (run panerusLima (2, 4, 0)) 58.68
    rightEqual (run panerusLima (3, 0, 0)) 62.18
    rightEqual (run panerusLima (4, 1, 0)) 75.68

test_note_call :: Test
test_note_call = do
    let derive scale control =
            DeriveTest.extract extract . derivePitch scale control
        extract e = (Score.initialNn e, Score.initialNote e)
    let f = derive "pelog lima"
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
        ([(Just (Pitch.modifyHz (+7) nn41), Just "41")], [])

    let gender = derive "pelog barang gender-panerus" ""
    let absolute = derive "pelog barang" ""
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

pelogLima :: Scale.Scale
pelogLima = ScaleTest.get "pelog lima"

pelogBarang :: Scale.Scale
pelogBarang = ScaleTest.get "pelog barang"

panerusLima :: Scale.Scale
panerusLima = ScaleTest.get "pelog lima gender-panerus"

panerusBarang :: Scale.Scale
panerusBarang = ScaleTest.get "pelog barang gender-panerus"

-- * util
--
-- TODO copy paste from Wayang_test

showScale :: Scale.Scale -> Pitch.Pitch -> Either Text Text
showScale scale = bimap pretty pretty . Scale.scaleShow scale mempty

readScale :: Scale.Scale -> Pitch.Note -> Either Text Pitch.Pitch
readScale scale = first pretty . Scale.scaleRead scale mempty

scaleTrack :: Text -> [Text] -> [UiTest.TrackSpec]
scaleTrack scaleId pitches =
    [ (">", [(n, 1, "") | n <- map fst events])
    , ("*" <> scaleId, [(n, 0, p) | (n, p) <- events])
    ]
    where events = zip (Lists.range_ 0 1) pitches

derivePitch :: Text -> Text-> Text -> Derive.Result
derivePitch scale control pitch =
    DeriveTest.deriveTracks ("scale " <> scale)
        [ (Texts.join2 " | " ">" control, [(0, 1, "")])
        , ("*", [(0, 0, pitch)])
        ]
