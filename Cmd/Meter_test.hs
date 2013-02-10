module Cmd.Meter_test where
import qualified Data.Map as Map
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Ruler as Ruler
import qualified Cmd.Meter as Meter


test_make_meter = do
    let meter = Meter.make_meter 1 [Meter.regular_subdivision [4, 4]]
    equal meter (Meter.marklist_meter (Meter.meter_marklist meter))

    let marks = Map.toList $ Ruler.marklist_map (Meter.meter_marklist meter)
    equal (map fst marks) (Seq.range 0 16 1)

test_ranks_to_label = do
    let f = Meter.ranks_to_labels
    equal (f [0, 2, 2, 1, 2, 2, 0])
        ["", "0.1", "0.2", "1", "1.1", "1.2", ""]
        -- ["1", "1.2", "1.3", "1.4", "2", "2.2", "2.3", "2.4", "3"]
