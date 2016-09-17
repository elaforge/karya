-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Meter_test where
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.Ruler as Ruler
import qualified Cmd.Meter as Meter
import qualified Cmd.Meters as Meters
import Global
import Types


test_make_meter = do
    let meter = Meter.make_meter 1 [Meter.regular_subdivision [4, 4]]
    equal meter (Meter.marklist_meter (Meter.meter_marklist config meter))

    let marks = Ruler.ascending 0 (Meter.meter_marklist config meter)
    equal (map fst marks) (Seq.range 0 16 1)

test_meter_marklist = do
    let f = extract_marklist 20 . Meter.meter_marklist config
            . Meter.fit_meter 64 . replicate 4
    equal (take 9 $ f Meters.m44_4) $ zip (Seq.range_ 0 1)
        [ "1", ".2", ".3", ".4"
        , "2", ".2", ".3", ".4"
        , "3"
        ]

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

test_rational_meter2 = do
    -- Even awkward fractions add up correctly.
    let meter = map fst $ Ruler.ascending 0 $ Meter.labeled_marklist $
            replicate 1025 $ Meter.LabeledMark 0 dur ""
        dur = 5/8 / 128
    equal (last meter) 5 -- dur*1024 == 5

test_renumber = do
    let modify :: Meter.LabeledMeter -> Meter.LabeledMeter
        modify = dropWhile ((>=2) . Meter.m_rank) . drop 1
        ruler = Meters.ruler $ Meter.meter_marklist Meter.default_config $
            Meter.fit_meter 4 [Meter.repeat 4 Meters.m34]
        extract = extract_marklist 20
    equal (extract . snd . Ruler.get_marklist Ruler.meter <$>
            Meter.modify_meter modify ruler) $
        Right (zip (Seq.range_ 0 1)
            (map Meter.biggest_label ["1", "2", "3", "4"]))

extract_marklist :: Double -> Ruler.Marklist -> [(ScoreTime, Text)]
extract_marklist zoom = mapMaybe name_of . Ruler.ascending 0
    where
    name_of (t, m)
        | Ruler.mark_name_zoom_level m <= zoom = Just (t, Ruler.mark_name m)
        | otherwise = Nothing

test_renumber_topmost = do
    let f = map Meter.m_label . Meter.renumber_topmost . mkmeter
        mkmeter marks =
            [Meter.LabeledMark rank 1 label | (rank, label) <- marks]
    let meter = [(0, "1"), (2, "1.2"), (0, "2"), (2, "2.2")]
    equal (f meter) (map snd meter)
    equal (f (meter ++ meter)) (map snd meter ++ ["3", "3.2", "4", "4.2"])

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

test_strip_prefixes = do
    let f depth = map Meter.join_label . Meter.strip_prefixes "-" depth
            . map Meter.split_label
    equal (f 2 ["1.1", "1.2", "1.2.1", "2"]) ["1.1", "-.2", "-.-.1", "2"]
    equal (f 1 ["1.1", "1.2", "1.2.1", "2"]) ["1.1", "-.2", "-.2.1", "2"]
    equal (f 0 ["1.1", "1.2", "1.2.1", "2"]) ["1.1", "1.2", "1.2.1", "2"]


config :: Meter.MeterConfig
config = Meter.default_config
    { Meter.config_label_components = Meter.number_components 1 1 }
