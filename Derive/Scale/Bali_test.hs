-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Bali_test where
import Util.Test
import qualified Derive.Scale.Bali as Bali
import qualified Perform.Pitch as Pitch


test_extend_scale = do
    let f = Bali.extend_scale 2
        p = Pitch.pitch
    equal (f (p 1 0) (p 2 0) (p 1 0) [12, 13]) [12, 13, 24]
    equal (f (p 1 0) (p 2 1) (p 1 0) [12, 13]) [12, 13, 24, 25]
    equal (f (p 0 0) (p 1 1) (p 1 0) [12, 13]) [0, 1, 12, 13]
    equal (f (p 0 0) (p 3 0) (p 1 0) [12, 13]) [0, 1, 12, 13, 24, 25, 36]
