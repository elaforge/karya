-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Meter_test where

import qualified Data.Text as Text

import qualified Ui.Meter as Meter
import qualified Ui.Meters as Meters
import qualified Ui.Ruler as Ruler

import           Global
import           Types
import           Util.Test


test_make_measures :: Test
test_make_measures = do
    let f zoom = extract_zoom zoom . Meter.make_measures config
    let marks = f 70 [(2, 1, Meters.m44), (2, 1, Meters.m34)]
    equal (map (Ruler.mark_rank . snd) marks)
        [ 0, 3, 2, 3, 1, 3, 2, 3
        , 0, 3, 3, 1, 3, 3
        , 0
        ]
    equal (map (second mark_name) marks)
        [ (0, "1"), (0.25, ".2"), (0.5, ".3"), (0.75, ".4")
        , (1, "2"), (1.25, ".2"), (1.5, ".3"), (1.75, ".4")
        , (2, "3"), (2+1/3, ".2"), (2+2/3, ".3")
        , (3, "4"), (3+1/3, ".2"), (3+2/3, ".3")
        , (4, "5")
        ]

test_to_rank_durations :: Test
test_to_rank_durations = do
    let f m = map fst $ Meter.to_rank_durations [(1, m)]
    equal (f (Meter.regular_subdivision [4, 4]))
        [ 0, 2, 2, 2, 1, 2, 2, 2
        , 1, 2, 2, 2, 1, 2, 2, 2
        , 0
        ]

{-

test_rational_meter :: Test
test_rational_meter = do
    -- Meters with 1/3 divisions don't get inaccurate.
    let f = Meter.meter_marklist config . Meter.fit_meter 4
        extract = extract_marklist 20
        round_trip = Meter.meter_marklist config . Meter.marklist_meter
        meter = [Meter.repeat 4 Meters.m34]
    equal (extract $ f meter)
        (zip (Seq.range_ 0 1) ["1", "2", "3", "4", "5"])
    equal (extract $ round_trip $ f meter)
        (zip (Seq.range_ 0 1) ["1", "2", "3", "4", "5"])

test_rational_meter2 :: Test
test_rational_meter2 = do
    -- Even awkward fractions add up correctly.
    let meter = map fst $ Ruler.ascending 0 $ Meter.labeled_marklist $
            replicate 1025 $ Meter.LabeledMark 0 dur ""
        dur = 5/8 / 128
    equal (last meter) 5 -- dur*1024 == 5
-}

test_apply_labels :: Test
test_apply_labels = do
    let f labels = map (Text.intercalate ".") . Meter.apply_labels labels
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
    let f depth = map Meter.join_label . Meter.strip_prefixes "-" depth
            . map Meter.split_label
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
    -- { Meter.config_label_components = Meter.number_components 1 }

extract_zoom :: Double -> [(a, Ruler.Mark)] -> [(a, Ruler.Mark)]
extract_zoom zoom = mapMaybe $ \(t, m) ->
    if Ruler.mark_name_zoom_level m <= zoom then Just (t, m)
        else Nothing

mark_name :: Ruler.Mark -> Text
mark_name = Meter.strip_markup . Ruler.mark_name

extract_marklist :: Double -> Meter.Marklist -> [(ScoreTime, Text)]
extract_marklist zoom = map (second mark_name) . extract_zoom zoom
