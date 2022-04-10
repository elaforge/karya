-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Control_test where
import Util.Test
import qualified Perform.Midi.Control as Control


test_pitch_to_midi :: Test
test_pitch_to_midi = do
    let f = Control.pitch_to_midi (-1, 1)
    equal (f 10) (Just (10, 0))
    -- Don't emit a pitch bend if it's close enough.
    equal (f 10.001) (Just (10, 0))
    equal (f 9.999) (Just (10, 0))
    equal (f 10.01) (Just (10, 0.01))
    equal (f 9.99) (Just (9, 0.99))
    equal (f 10.5) (Just (10, 0.5))
    -- Shouldn't get NaN if pitch bend range is 0.
    equal (Control.pitch_to_midi (0, 0) 10.5) (Just (10, 0))
