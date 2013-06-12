-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Meter_test where
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Ruler as Ruler
import qualified Cmd.Meter as Meter
import qualified Cmd.Meters as Meters


test_make_meter = do
    let meter = Meter.make_meter 1 [Meter.regular_subdivision [4, 4]]
    equal meter (Meter.marklist_meter (Meter.meter_marklist meter))

    let marks = Ruler.ascending 0 (Meter.meter_marklist meter)
    equal (map fst marks) (Seq.range 0 16 1)

test_meter_marklist = do
    let f = extract . Meter.meter_marklist . Meter.fit_meter 64 . replicate 4
        extract = filter ((<=2) . fst) . map (extract_mark . snd)
            . Ruler.ascending 0
        extract_mark m = (Ruler.mark_rank m, Ruler.mark_name m)
    pprint (take 9 $ f Meters.m44_4)

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
