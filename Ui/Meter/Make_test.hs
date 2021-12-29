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

import           Global
import           Types
import           Util.Test


test_make_measures :: Test
test_make_measures = do
    let make zoom = extract_zoom zoom . Make.make_measures
    let marks = make 70 $ make_meter [(2, 1, Meters.m44), (2, 1, Meters.m34)]
    let extract = map (second mark_name)
    equal (map (Mark.mark_rank . snd) marks)
        [ 0, 3, 2, 3, 1, 3, 2, 3
        , 0, 3, 3, 1, 3, 3
        , 0
        ]
    equal (extract marks)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2"), (1.5, ".3"), (1.75, ".4")
        , (2, "3"), (2+1/3, ".2"), (2+2/3, ".3")
        , (3, "4"), (3+1/3, ".2"), (3+2/3, ".3")
        , (4, "5")
        ]

test_clip_end :: Test
test_clip_end = do
    let make = extract_zoom 70 . Make.make_measures
    let mm = make_meter [(1, 1, Meters.m44), (1, 0.75, Meters.m34)]
    let extract = map (second mark_name)
    let f = Meter.clip_end
    equal (extract $ make $ f 10 mm)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2"), (1.5, ".3")
        , (1.75, "3")
        ]
    equal (Meter.meter_end $ f 10 mm) 1.75
    equal (extract $ make $ f 1.5 mm)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2"), (1.5, ".3")
        ]
    equal (extract $ make $ f 0.5 mm)
        [(0, "1"), (0.25, ".2"), (0.5, ".3")]
    equal (Meter.meter_end $ f 0.5 mm) 0.5
    equal (extract $ make $ f 0 mm) [(0, "1")]
    equal (Meter.meter_end $ f 0 mm) 0

test_clip_start :: Test
test_clip_start = do
    let f t = map (second mark_name) $ extract_zoom 70 $ Make.make_measures $
            Meter.clip_start t mm
        mm = make_meter [(1, 1, Meters.m44), (1, 0.75, Meters.m34)]
    equal (f 0)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2"), (1.5, ".3")
        , (1.75, "3")
        ]
    -- TODO should be .3 .4, but clipping by trimming the AbstractMeter
    -- fundamentally can't represent this!  I guess I'll have to do it like
    -- clip_end after all.
    equal (f 0.5) $
        map (first (subtract 0.5))
        -- [ (0.5, ".3"), (0.75, ".4")
        [ (0.5, "1"), (0.75, ".2")
        , (1, "2"), (1.25, ".2"), (1.5, ".3")
        , (1.75, "3")
        ]

make_meter :: [(Meter.Measures, Meter.Duration, Meter.AbstractMeter)]
    -> Meter.Meter
make_meter sections =
    Meter.modify_sections (const ms) Meter.empty_meter
    where ms = map (Control.uncurry3 Meter.Section) sections

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
    equal (extract_marklist 20 $ Make.make_measures meter)
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
    equal (f 2 ["1.1", "1.2", "1.2.1", "2"]) ["1.1", "-.2", "-.-.1", "2"]
    equal (f 1 ["1.1", "1.2", "1.2.1", "2"]) ["1.1", "-.2", "-.2.1", "2"]
    equal (f 0 ["1.1", "1.2", "1.2.1", "2"]) ["1.1", "1.2", "1.2.1", "2"]

{-
test_label_meter :: Test
test_label_meter = do
    let f = map (Meter.strip_markup . Meter.m_label) $
            Meter.label_meter config (Meter.make_meter 1 [meter])
        meter = Meter.regular_subdivision [2, 2, 2, 4]
        config = Gong.config
            { Meter.config_labeled_ranks = Set.fromList [Meter.Section, Meter.Q]
            }
        -- section: gong, 1: gong stroke, 2: jegog, 4: calung, 8: kotekan*2,
        -- 16: kotekan*4, ...
    -- TODO why is the .0.1 different?
    equal f
        [ "1", ".0.1", "..2", "..3"
        , ".1", "..1", "..2", "..3"
        , ".2", "..1", "..2", "..3"
        , ".3", "..1", "..2", "..3"
        , ".4", "..1", "..2", "..3"
        , ".5", "..1", "..2", "..3"
        , ".6", "..1", "..2", "..3"
        , ".7", "..1", "..2", "..3"
        , "2"
        ]
-}

config :: Meter.Config
config = Meter.default_config

extract_zoom :: Double -> [(a, Mark.Mark)] -> [(a, Mark.Mark)]
extract_zoom zoom = filter ((<= zoom) . Mark.mark_name_zoom_level . snd)

mark_name :: Mark.Mark -> Text
mark_name = Make.strip_markup . Mark.mark_name

extract_marklist :: Double -> [(TrackTime, Mark.Mark)] -> [(TrackTime, Text)]
extract_marklist zoom = map (second mark_name) . extract_zoom zoom
