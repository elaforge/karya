module Cmd.Selection_test where

import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Types as Types

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Selection as Selection


test_events_around = do
    let e_sel (_track_id, (before, within, after)) =
            (map fst before, map fst within, map fst after)
    let f evts sel_start sel_end = CmdTest.extract (map e_sel) id $
            CmdTest.run_tracks2 [("i", mkspec evts)] $ do
                CmdTest.set_sel 1 sel_start 1 sel_end
                Selection.events_around
    equal (f [(0, 1), (2, 1)] 1 1) $ Right (Just [([], [0], [2])], [])
    equal (f [(1, -1), (3, -1)] 2 2) $ Right (Just [([1], [3], [])], [])
    -- positive events win when there are both choices
    equal (f [(0, 1), (3, -1)] 2 2) $ Right (Just [([], [0], [3])], [])
    -- no one wins when there are no choices
    equal (f [(1, -1), (3, 1)] 2 2) $ Right (Just [([1], [], [3])], [])

    equal (f [(0, 1), (1, 1), (2, 1)] 0.5 2.5) $ Right
        (Just [([0], [1, 2], [])], [])

mkspec specs = [(p, d, show p) | (p, d) <- specs]

test_get_time_offset = do
    let bid = CmdTest.default_block_id
    let mkview h = (Block.view bid (Types.Rect 0 0 10 (h+10)) (Types.Zoom 0 1))
            { Block.view_visible_time = h }
    let f = Selection.get_time_offset 0.2 (mkview 20)

    equal (f 5 15) 0
    equal (f 15 25) 5
    equal (f 10 35) 6
