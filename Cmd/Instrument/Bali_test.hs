-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Instrument.Bali_test where
import Util.Test
import qualified Cmd.Instrument.Bali as Bali
import qualified Derive.Scale as Scale
import qualified Perform.Pitch as Pitch


test_wrap_octaves = do
    let f low high = unpitch
            . Bali.wrap_octaves (Scale.Range (pitch low) (pitch high)) . pitch
        pitch = uncurry Pitch.pitch
        unpitch p = (Pitch.pitch_octave p, Pitch.pitch_pc p)
    let f12 = f (1, 1) (2, 1)
    equal (f12 (3, 1)) (2, 1)
    equal (f12 (3, 2)) (1, 2)
    equal (f12 (5, 2)) (1, 2)
    equal (f12 (1, 0)) (2, 0)
    equal (f12 (0, 1)) (1, 1)
    equal (f12 (-5, 3)) (1, 3)
