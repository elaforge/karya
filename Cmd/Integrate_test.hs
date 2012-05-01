module Cmd.Integrate_test where
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Integrate as Integrate
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Score as Score


test_integrate = do
    let f = first (map extract) . Integrate.integrate lookup Nothing
            . map DeriveTest.mkevent
        lookup k = Map.lookup k Scale.All.scales
        extract (Integrate.Track title events) =
            (title, map UiTest.extract_event events)
    equal (f [(0, 1, "a", [], Score.Instrument "inst")])
        ([(">inst", [(0, 1, "a")]), ("*twelve", [(0, 0, "4c")])], [])
