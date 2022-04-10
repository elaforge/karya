-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Skeleton_test where
import Util.Test
import qualified Ui.Skeleton as Skeleton


test_move :: Test
test_move = do
    let f from to = Skeleton.flatten . Skeleton.move from to . Skeleton.make
    -- let f from to = Skeleton.move from to
    -- move up
    equal (f 0 1 []) []
    equal (f 1 2 [(1, 2)]) [(2, 1)]
    equal (f 0 2 [(0, 1)]) [(2, 0)]
    equal (f 1 2 [(1, 3)]) [(2, 3)]
    equal (f 2 3 [(0, 1), (2, 3), (4, 5)])
        [(0, 1), (3, 2), (4, 5)]

    -- move down
    equal (f 2 1 [(1, 2)]) [(2, 1)]
    equal (f 2 0 [(0, 1)]) [(1, 2)]
    equal (f 1 0 [(1, 2)]) [(0, 2)]
    equal (f 2 0 [(1, 2)]) [(2, 0)]

    -- multiple parents and children
    equal (f 3 2 [(1, 3), (2, 3)]) [(1, 2), (3, 2)]
    equal (f 2 3 [(1, 3), (2, 3)]) [(1, 2), (3, 2)]
