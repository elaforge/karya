module Cmd.Selection_test where

import Util.Test

import Ui
import qualified Ui.Block as Block
import qualified Ui.Types as Types

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Selection as Selection


test_get_time_offset = do
    let bid = CmdTest.default_block_id
    let mkview h = (Block.view bid (Types.Rect 0 0 10 (h+10)) (Types.Zoom 0 1))
            { Block.view_visible_time = h }
    let f = Selection.get_time_offset 0.2 (mkview 20)

    equal (f 5 15) 0
    equal (f 15 25) 5
    equal (f 10 35) 6
