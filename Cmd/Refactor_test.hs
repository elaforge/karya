-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Refactor_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Refactor as Refactor


test_selection_at = do
    let f start end = Refactor.selection_at "sub" parent
            [1] [UiTest.mk_tid_block parent 1] start end
        run parent subs start end = UiTest.extract_all_tracks $
            UiTest.exec (UiTest.run_mkblocks (("b", [(">", parent)]) : subs))
                (f start end)
        parent = UiTest.bid "b"
    equal (run [(0, 1, "a"), (1, 1, "b")] [] 0 1)
        [ (parent, [(">", [(0, 1, "sub"), (1, 1, "b")])])
        , (UiTest.bid "sub", [(">", [(0, 1, "a")])])
        ]
