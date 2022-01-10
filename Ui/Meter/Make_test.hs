-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Meter.Make_test where

import qualified Data.Text as Text

import qualified Util.Control as Control
import qualified Util.Seq as Seq
import qualified Ui.Meter.Make as Make
import qualified Ui.Meter.Mark as Mark
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Meter.Meters as Meters
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_make_measures :: Test
test_make_measures = do
    let marks = UiTest.meter_zoom 70 $
            make_meter [(2, 1, Meters.m44), (2, 1, Meters.m34)]
    equal (map (fst . snd) marks)
        [ 0, 3, 2, 3, 1, 3, 2, 3
        , 0, 3, 3, 1, 3, 3
        , 0
        ]
    equal (map (second snd) marks)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2"), (1.5, ".3"), (1.75, ".4")
        , (2, "3"), (2+1/3, ".2"), (2+2/3, ".3")
        , (3, "4"), (3+1/3, ".2"), (3+2/3, ".3")
        , (4, "5")
        ]

test_sections_take :: Test
test_sections_take = do
    let f t = UiTest.meter_marklist 70 $
            Meter.modify_sections (Meter.sections_take t) mm
        mm = make_meter [(1, 1, Meters.m44), (1, 0.75, Meters.m34)]
    equal (f 10)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2"), (1.5, ".3")
        , (1.75, "3")
        ]
    equal (f 1.5)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2")
        , (1.5, "3")
        ]
    equal (f 0.5) [(0, "1"), (0.25, ".2"), (0.5, "2")]
    equal (f 0) [(0, "1")]
    let make t = Make.make_measures $
            Meter.modify_sections (Meter.sections_take t) mm
    -- starts and ends with rank 0 even if it's cut short
    equal (map (Mark.mark_rank . snd) (make 0.25))
        [0, 6, 5, 6, 4, 6, 5, 6, 0]

test_sections_drop :: Test
test_sections_drop = do
    let f t = UiTest.meter_marklist 70 $
            Meter.modify_sections (Meter.sections_drop t) mm
        mm = make_meter [(1, 1, Meters.m44), (1, 0.75, Meters.m34)]
    equal (f 0)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2"), (1.5, ".3")
        , (1.75, "3")
        ]
    equal (f 0.5) $
        map (first (subtract 0.5))
        -- TODO: for a pickup, it should be
        --   (0.5, ".3"), (0.75, ".4")
        -- but clipping by trimming the AbstractMeter fundamentally can't
        -- represent this!
        [ (0.5, "1"), (0.75, ".2")
        , (1, "2"), (1.25, ".2"), (1.5, ".3")
        , (1.75, "3")
        ]

make_meter :: [(Meter.Measures, Meter.Duration, Meter.AbstractMeter)]
    -> Meter.Meter
make_meter sections =
    Meter.modify_sections (const ms) Meter.empty_meter
    where ms = map (Control.uncurry3 Meter.MSection) sections

test_to_rank_durations :: Test
test_to_rank_durations = do
    let f m = map fst $ Make.to_rank_durations [(1, m)]
    equal (f (Meter.regular_subdivision [4, 4]))
        [ 0, 2, 2, 2, 1, 2, 2, 2
        , 1, 2, 2, 2, 1, 2, 2, 2
        , 0
        ]

test_rational_meter :: Test
test_rational_meter = do
    -- Meters with 1/3 divisions don't get inaccurate.
    let meter = make_meter [(4, 1, Meters.m34)]
    equal (UiTest.meter_marklist 20 meter)
        (zip (Seq.range_ 0 1) ["1", "2", "3", "4", "5"])

test_rational_meter2 :: Test
test_rational_meter2 = do
    -- Even awkward fractions add up correctly.
    let meter = map fst $ Make.labeled_marklist 0 $
            replicate 1025 $ Make.LabeledMark 0 dur ""
        dur = 5/8 / 128
    equal (last meter) 5 -- dur*1024 == 5

test_apply_labels :: Test
test_apply_labels = do
    let f labels = map (Text.intercalate ".") . Make.apply_labels labels
    equal (f [] [0, 0]) ["", ""]
    let labels =
            [ map Text.singleton ['A'..'Z']
            , ["0", "1"]
            , map Text.singleton ['a'..'z']
            ]
    equal (f labels [0, 1, 0, 1]) ["A.0.a", "A.1.a", "B.0.a", "B.1.a"]
    -- I get "" when I run out of labels.
    equal (f labels [0, 1, 1, 1, 0, 1, 1, 1])
        [ "A.0.a", "A.1.a", "A..a", "A..a"
        , "B.0.a", "B.1.a", "B..a", "B..a"
        ]

    let labels =
            [ map Text.singleton ['A'..'C']
            , map Text.singleton ['a'..'c']
            , map showt [1..4]
            ]
    equal (f labels [0, 2, 1, 2, 1, 2, 0])
        ["A.a.1", "A.a.2", "A.b.1", "A.b.2", "A.c.1", "A.c.2", "B.a.1"]

test_strip_prefixes :: Test
test_strip_prefixes = do
    let f depth = map Make.join_label . Make.strip_prefixes "-" depth
            . map Make.split_label
    equal (f 2 ["1.1", "1.2", "1.2.1", "2"]) ["-.1", "-.2", "-.-.1", "2"]
    equal (f 1 ["1.1", "1.2", "1.2.1", "2"]) ["-.1", "-.2", "-.2.1", "2"]
    equal (f 0 ["1.1", "1.2", "1.2.1", "2"]) ["1.1", "1.2", "1.2.1", "2"]

config :: Meter.Config
config = Meter.default_config

mark_name :: Mark.Mark -> Text
mark_name = Make.strip_markup . Mark.mark_name
