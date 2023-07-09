-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Java_test where
import qualified Util.Lists as Lists
import qualified Util.ParseText as ParseText
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Java as Java
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Scale.TheoryFormat as TheoryFormat

import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_read :: Test
test_read = do
    -- let f scale key pitch = read_scale scale key pitch
    let f scale = read_scale scale "nokey" -- TODO remove key
    let unparseable n = Left $ "unparseable note: " <> n
    let mknote5 = Pitch.Note . ("5"<>) . showt
    -- .      1   2   3   4   5   6   7   1^
    -- lima   00  10  20  2#  30  40  4#  00
    -- barang 4#  00  10  1#  20  30  40  4#
    equal (f pelog_lima "50") (unparseable "50")
    equal (f pelog_lima "51") (Right "5-0")
    equal (f pelog_lima "58") (unparseable "58")

    -- 54 -> RelativePitch 5 3 Nothing -> Pitch 5 2 1
    equal (f pelog_lima "54") (Right "5-2#")
    -- 12356
    equal (map (f pelog_lima . mknote5) [1..7]) $ map Right
        ["5-0", "5-1", "5-2", "5-2#", "5-3", "5-4", "5-4#"]

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
    equal (f pelog_barang "51") (Right "4-4#")
    equal (f pelog_barang "52") (Right "5-1")
    equal (map (f pelog_barang . mknote5) [1..7]) $ map Right
        ["5-0", "5-1", "5-2", "5-2#", "5-3", "5-4", "5-4#"]
        -- ["5-0", "5-1", "5-2", "5-2#", "5-3", "5-4", "5-4#"]

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

test_input_to_note :: Test
test_input_to_note = do
    let f scale = ScaleTest.input_to_note scale mempty
        x = "invalid input"
    -- equal (f pelog_lima (5, 0, 0)) "51"
    -- equal (f pelog_lima (5, 1, 0)) "52"
    -- equal (f pelog_lima (5, 2, 0)) "53"
    -- equal (f pelog_lima (5, 2, 1)) "54"
    -- equal (f pelog_lima (5, 3, 0)) "55"
    -- equal (f pelog_lima (5, 4, 0)) "56"
    -- equal (f pelog_lima (5, 5, 0)) "61"
    --
    -- equal (map (f pelog_lima) [(5, pc, acc) | pc <- [0..5], acc <- [0, 1]])
    --     [ "51", x
    --     , "52", x
    --     , "53", "54"
    --     , "55", x
    --     , "56", "57"
    --     , "61", x
    --     ]

    -- 23567
    equal (map (f pelog_barang) [(5, pc, acc) | pc <- [0..5], acc <- [0, 1]])
        [ "52", x
        , "53", "54"
        , "55", x
        , "56", x
        , "57", "61"
        , "62", x
        ]

    {-
        Theory.layout [1, 1, 2, 1, 2] -- 12356
         x  x  4  x  7
        1  2  3  5  6  1

        Theory.layout [1, 2, 1, 1, 2] -- 23567
         x  4   x  x  1
        2  3  5  6  7
        0  1  2  3  4

        but got
          x 53  x  x 57  x
        51 52 54 55 56 61

        Pattern right, numbers wrong.  Need to add one Step to each one.
        Add one PC wrong because skips according to layout.
        No it's right, but Pitch doesn't map like I expect?
            50  -> 51   52
            50# ->      x -- actual: 50# -> 51# 53
            51  -> 52   53 --        51  -> 52  54
            51# -> 52#  54
            52  -> 53   55
            52# ->      x
            53  -> 54   56
            53# ->      x
            54  -> 55   57
            54# -> 55#  61

    -}

    -- 01234
    -- 23567
    equal (f pelog_barang (5, 4, 0)) "57"

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
    equal (run pelog_lima (5, 0, 0)) (Right (Right 74.25))
    equal (run pelog_lima (5, 1, 0)) (Right (Right 75.68))
    -- equal (run panerus (3, 4, 0)) (Right (Left DeriveT.InvalidInput))
    -- equal (run panerus (3, 5, 0)) (Right (Right 58.68))
    -- equal (run panerus (4, 0, 0)) (Right (Right 62.18))
    -- equal (run panerus (5, 1, 0)) (Right (Right 75.68))

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
