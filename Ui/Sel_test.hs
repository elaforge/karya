-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Sel_test where
import Util.Test
import qualified Ui.Sel as Sel


test_union :: Test
test_union = do
    let f = Sel.union
        sel st sp ct cp = Sel.Selection st sp ct cp Sel.Positive
    equal (f (sel 1 1 2 2) (sel 3 3 4 4)) (sel 1 1 4 4)
    equal (f (sel 3 3 4 4) (sel 1 1 2 2)) (sel 4 4 1 1)
