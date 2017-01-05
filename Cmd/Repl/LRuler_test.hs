-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Repl.LRuler_test where
import Util.Test
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create
import qualified Cmd.Repl.LRuler as LRuler
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Ruler.RulerUtil as RulerUtil

import Types


test_extract = do
    let meter_ruler = RulerUtil.meter_ruler Meter.default_config
    let ((vid, bid), ui_state) = UiTest.run Ui.empty $ do
            [top, b1, b2] <- UiTest.mkblocks
                [ ("top", [(">", [(0, 10, "b1"), (10, 6, "b2")])])
                , ("b1", [])
                , ("b2", [])
                ]
            vid <- Create.unfitted_view top
            Create.new_ruler top "r.top" $ meter_ruler 16 []
            Create.new_ruler b1 "r.b1" $ meter_ruler 16 [Meter.repeat 4 Meter.T]
            Create.new_ruler b2 "r.b2" $ meter_ruler 16 [Meter.repeat 3 Meter.T]
            return (vid, top)
    equal (e_ruler bid ui_state) [(0, 0)]
    res <- CmdTest.run_ui_io ui_state $ do
        CmdTest.set_sel_on vid 1 0 1 0
        LRuler.modify LRuler.extract
    equal (CmdTest.result_val res) (Right (Just ()))
    equal (e_ruler bid (CmdTest.result_ui_state res))
        [ (0, 2.5), (1, 2.5), (1, 2.5), (1, 2.5)
        , (0, 2), (1, 2), (1, 2)
        , (0, 0)
        ]

e_ruler :: BlockId -> Ui.State -> Meter.Meter
e_ruler bid ustate = UiTest.eval ustate $
    Meter.unlabel_meter . Meter.ruler_meter <$>
        (Ui.get_ruler =<< Ui.ruler_of bid)
