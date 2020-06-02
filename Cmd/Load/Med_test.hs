-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Load.Med_test where
import Util.Test
import qualified Cmd.Load.Med as Med
import qualified Cmd.Load.ModTypes as M


test_default_zeroes = do
    let f = map extract . Med.default_zeroes . map make
        make (inst, cmds) = M.Line Nothing inst cmds
        extract (M.Line _ inst cmds) = (inst, cmds)
    equal (f [(1, []), (0, [])]) [(1, []), (1, [])]
    equal (f [(1, []), (2, []), (0, [])]) [(1, []), (2, []), (2, [])]

    let f2 = map snd . f . map (1,)
    let c1 = M.Command "1"
        c2 = M.Command "2"
    equal (f2 [[c1 1], [c1 0]]) [[c1 1], [c1 1]]
    equal (f2 [[c1 1, c2 2], [c1 0, c2 0]]) [[c1 1, c2 2], [c1 1, c2 2]]
    equal (f2 [[M.Volume 1, c1 1], [M.Volume 0, c1 0]])
        [[M.Volume 1, c1 1], [M.Volume 0, c1 1]]
