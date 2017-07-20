-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Octa_test where
import Util.Test
import qualified Ui.Ui as Ui
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Octa as Octa
import qualified Derive.Scale.ScaleTest as ScaleTest

import qualified Perform.NN as NN
import Global


test_input_to_nn = do
    let octa21 = ScaleTest.get_scale Octa.scales "octa21"
    let f input = second (first prettys) $ DeriveTest.eval Ui.empty $
            Scale.scale_input_to_nn octa21 0 input
    let ascii oct = CmdTest.ascii_kbd . CmdTest.oct_pc oct
    equal [f (ascii 4 n) | n <- [0..8]] $ map (Right . Right)
        [NN.c4, NN.d4, NN.ds4, NN.f4, NN.fs4, NN.gs4, NN.a4, NN.b4, NN.c5]
