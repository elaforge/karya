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
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


-- .      1   2   3   4   5   6   7   1^
-- lima   00  10  20  21  30  40  41  00
-- barang 41  00  10  11  20  30  40  41
-- barang 01  10  20  21  30  40  00  01
-- Step   0   1   2   3   4   5   6   0
--        00  10  11  20  30  40  41  00

test_read :: Test
test_read = do
    -- let f scale key pitch = read_scale scale key pitch
    let f scale = fmap pretty . read_scale scale
    let unparseable n = Left $ "unparseable note: " <> n
    -- .      1   2   3   4   5   6   7   1^
    -- lima   00  10  20  2#  30  40  4#  00
    -- barang 4#  00  10  1#  20  30  40  4#
    equal (f pelog_lima "50") (unparseable "50")
    equal (f pelog_lima "51") (Right "5-0")
    equal (f pelog_lima "58") (unparseable "58")

    -- 54 -> RelativePitch 5 3 Nothing -> Pitch 5 2 1
    -- equal (f pelog_lima "54") (Right "5-2#")
    -- 12356
    pprint (map (f pelog_lima . note4) [1..7])
    -- equal (map (f pelog_lima . note4) [1..7]) $ map Right
    --     ["5-0", "5-1", "5-2", "5-3", "5-4", "5-5", "5-6"]
    --     -- ["5-0", "5-1", "5-2", "5-2#", "5-3", "5-4", "5-4#"]

    {-
        Pitch is absolute.  So a relative input will be adjusted to absolute.
        "51" is absolute, but it's a Pitch with accidental: 4-5#
        "52" -> 5-0
        "53" -> 5-2
        "54" has accidental: 5-2#
        "55" is one pc above "53": 5-3
        "56" +1 pc: 5-4
        "57" +1 pc: 5-5
        "61": 5-5#
        "62": 6-0

        This is all weird because the missing 1 forces Pitch to all be adjusted
        down one.  If I take relative twelve as a model, why doesn't it work?
        D-maj doesn't have C in it.  So:
            4n -> 5c# 5-0#
            5s -> 5d  5-1
            5r -> 5e  5-2
            5g -> 5f# 5-3#
            5m -> 5g  5-4
            5p -> 5a  5-5
            5d -> 5b  5-6
            5n -> 6c# 6-0#
            6s -> 6d  6-1
        This is actually absolute, you can't transpose this without knowing the
        key.  It's just the parsed structure of the pitch that it would have
        been in absolute.  So I don't have the property that Pitch can be
        transposed, you have to use scale_transpose.
        No I'm wrong, it is transposeable, diatonic transposition is just
        adding letters.

        What do I use Pitch for anyway?
        - lilypond convert twelve
        - Val:
        - Highlight:
        - Bali.Gangsa:

        If Pitch is just parsed version of Note, then why not no accidentals?
        It doesn't reflect the layout, only diatonic transposition and keyboard
        entry would.  Would that make it simpler?
    -}
    -- 51 -> 4#
    -- [1, 2, 1, 1, 2] -- 23567
    -- 23567 -- 12456
    -- equal (f pelog_barang "51") (Right "4-4#")
    -- equal (f pelog_barang "52") (Right "5-1")
    -- equal (map (f pelog_barang . note4) [1..7]) $ map Right
    --     ["5-0", "5-1", "5-2", "5-2#", "5-3", "5-4", "5-4#"]
    --     -- ["5-0", "5-1", "5-2", "5-2#", "5-3", "5-4", "5-4#"]

    -- equal (f panerus lima "1") (Right "5-0")
    -- equal (f panerus lima "2") (Right "5-1")
    -- equal (f panerus lima "`1^`") (Right "6-0")
    -- -- Notation is absolute, even if input is relative to key.
    -- equal (f panerus barang "1") (Right "5-0")
    --
    -- -- equal (f "wayang-pemade" "i^") (Right "5-0")
    -- -- equal (f "wayang-kantilan" "i-") (Right "5-0")
    -- -- let run scale pitch = DeriveTest.extract Score.initial_nn $
    -- --         DeriveTest.derive_tracks "" $ scale_track scale [pitch]
    -- -- equal (run "wayang" "5i") (run "wayang-pemade" "i^")
    -- -- equal (run "wayang" "6i") (run "wayang-kantilan" "i^")

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
    -- equal (f pelog_lima (4, 0, 0)) "41"
    -- equal (f pelog_lima (4, 1, 0)) "42"
    -- equal (f pelog_lima (4, 2, 0)) "43"
    -- equal (f pelog_lima (4, 2, 1)) "44"
    -- equal (f pelog_lima (4, 3, 0)) "45"
    -- equal (f pelog_lima (4, 4, 0)) "46"
    -- equal (f pelog_lima (4, 5, 0)) "51"
    --
    -- equal (map (f pelog_lima) [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]])
    --     [ "41", x
    --     , "42", x
    --     , "43", "44"
    --     , "45", x
    --     , "46", "47"
    --     , "51", x
    --     ]
    --
    -- equal (f pelog_barang (4, 4, 0)) "47"
    -- equal (f pelog_barang (4, 4, 1)) "51"
    -- -- 23567
    -- equal (map (f pelog_barang) [(4, pc, acc) | pc <- [0..5], acc <- [0, 1]])
    --     [ "42", x
    --     , "43", "44"
    --     , "45", x
    --     , "46", x
    --     , "47", "51"
    --     , "52", x
    --     ]

    equal (f panerus (4, 0, 0)) "1"
    equal (f panerus (4, 1, 0)) "2"

    equal (f panerus (5, 0, 0)) "`1^`"
    equal (f panerus (5, 2, 0)) "`3^`"
    equal (f panerus (5, 3, 0)) x
    equal (f panerus (3, 4, 0)) "`6.`"
    equal (f panerus (2, 4, 0)) "`6..`"
    equal (f panerus (2, 3, 0)) x

    -- -- 01234
    -- -- 23567
    -- equal (f pelog_barang (5, 4, 0)) "57"

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
    -- equal (run pelog_lima (4, 0, 0)) (Right (Right 74.25))
    -- equal (run pelog_lima (4, 1, 0)) (Right (Right 75.68))
    equal (run panerus (2, 3, 0)) (Right (Left DeriveT.InvalidInput))
    -- equal (run panerus (2, 4, 0)) (Right (Right 58.68))
    -- equal (run panerus (3, 0, 0)) (Right (Right 62.18))
    equal (run panerus (4, 1, 0)) (Right (Right 75.68))

test_note_call :: Test
test_note_call = do
    let f control =
            DeriveTest.extract extract . derive_pitch "pelog-lima" "" control
        extract e = (Score.initial_nn e, Score.initial_note e)
    equal (f "" "41") ([(Just 74.25, Just "41")], [])
    equal (f "t-chrom=1" "41") ([(Just 75.68, Just "42")], [])
    equal (f "t-chrom=3" "41") ([(Just 80, Just "44")], [])
    equal (f "t-dia=1" "41") ([(Just 75.68, Just "42")], [])
    -- symbolic pitch rounds to 54, but it's actually a bit higher.
    equal (f "t-dia=2.6" "41") ([(Just 80.206, Just "44")], [])
    equal (f "t-dia=3" "41") ([(Just 81.03, Just "45")], [])
    equal (f "t-dia=-1" "41") ([(Just 70.5, Just "36")], [])
    equal (f "t-oct=1" "41") ([(Just 86.4, Just "51")], [])
    equal (f "t-nn=1" "41") ([(Just (74.25 + 1), Just "41")], [])
    equal (f "t-hz=7" "41")
        ([(Just (Pitch.modify_hz (+7) 74.25), Just "41")], [])

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

panerus :: Scale.Scale
panerus = get_scale "pelog-lima-gender-panerus"

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
