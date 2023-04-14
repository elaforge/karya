-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Factor_test where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Factor as Factor
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_selection_alts :: Test
test_selection_alts = do
    let run relative = CmdTest.run_tracks tracks $ do
            CmdTest.set_sel 1 1 1 3
            Factor.selection_alts relative 3 (UiTest.mkid "sub")
        tracks = UiTest.regular_notes 4
    let get_block = Lists.head
            <=< UiTest.extract_block_id UiTest.default_block_id
            . CmdTest.result_ui_state
    let result = run True
    equal (CmdTest.result_val result)
        (Right $ Just $ map UiTest.bid ["b1-sub1", "b1-sub2", "b1-sub3"])
    equal (get_block result)
        (Just (">", [(0, 1, ""), (1, 2, "alt -sub1 -sub2 -sub3"), (3, 1, "")]))
    equal (Map.keys (Ui.state_views (CmdTest.result_ui_state result)))
        (map UiTest.vid ["b1-sub1.v1", "b1-sub2.v1", "b1-sub3.v1", "v.b1"])

    let result = run False
    equal (CmdTest.result_val result)
        (Right $ Just $ map UiTest.bid ["sub1", "sub2", "sub3"])
    equal (get_block result)
        (Just (">", [(0, 1, ""), (1, 2, "alt sub1 sub2 sub3"), (3, 1, "")]))

test_selection :: Test
test_selection = do
    let run tracks subs start end = UiTest.extract_blocks $
            CmdTest.result_ui_state $ CmdTest.run_ui Ui.empty $ do
                parent_v : _ <- UiTest.mkviews (("b", [(">", tracks)]) : subs)
                CmdTest.set_sel_on parent_v 1 start 1 end
                Factor.selection_ True False (UiTest.mkid "sub")
    equal (run [(0, 1, "a"), (1, 1, "b")] [] 0 1)
        [ ("b", [(">", [(0, 1, "sub"), (1, 1, "b")])])
        , ("sub", [(">", [(0, 1, "a")])])
        ]
