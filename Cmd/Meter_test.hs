module Cmd.Meter_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.Ruler as Ruler
import qualified Cmd.Meter as Meter


test_make_meter = do
    let meter = Meter.make_meter 1 (Meter.regular_subdivision [4, 4])
    equal meter (Meter.marklist_meter (Meter.meter_marklist meter))

    let marks = Ruler.marks_of (Meter.meter_marklist meter)
    equal (map fst marks) (Seq.range 0 16 1)
