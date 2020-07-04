-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Load.ModT_test where
import qualified Data.IntMap as IntMap

import qualified Cmd.Load.ModT as ModT

import           Util.Test


test_carry_zeroes :: Test
test_carry_zeroes = do
    let f = map extract . IntMap.elems . ModT.carry_zeroes
            . IntMap.fromList . zip [0..] . map make
        make (inst, cmds) = ModT.Line Nothing inst cmds
        extract (ModT.Line _ inst cmds) = (inst, cmds)
    equal (f [(1, []), (0, [])]) [(1, []), (1, [])]
    equal (f [(1, []), (2, []), (0, [])]) [(1, []), (2, []), (2, [])]

    let f2 = map snd . f . map (1,)
    let c1 = ModT.Command "1"
        c2 = ModT.Command "2"
    equal (f2 [[c1 1], [c1 0]]) [[c1 1], [c1 1]]
    equal (f2 [[c1 1, c2 2], [c1 0, c2 0]]) [[c1 1, c2 2], [c1 1, c2 2]]
    equal (f2 [[ModT.Volume 1, c1 1], [ModT.Volume 0, c1 0]])
        [[ModT.Volume 1, c1 1], [ModT.Volume 0, c1 1]]

    let slide = ModT.VolumeSlide
    equal (f2 [[slide 1], [slide 0], [slide 0]])
        [[slide 1], [slide 1], [slide 1]]
