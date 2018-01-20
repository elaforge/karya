-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.MSignal_test where
import Util.Test
import qualified Util.TimeVector as TimeVector
import qualified Perform.Midi.MSignal as MSignal


test_pitches_share = do
    let sample0 ((_, y) : _) = fromIntegral (floor y)
        sample0 _ = error "empty"
    let f start end sig1 sig2 = MSignal.pitches_share False start end
            (sample0 sig1) (from_pairs sig1)
            (sample0 sig2) (from_pairs sig2)
        from_pairs = TimeVector.signal

    -- Different signals.
    equal (f 0 1 [(0, 0), (1, 1)] [(0, 1), (1, 0)]) False
    -- Separated by an integer can share.
    equal (f 0 1 [(0, 0), (1, 1)] [(0, 1), (1, 2)]) True
    -- Separated by 0 can't share.
    equal (f 0 1 [(0, 0), (1, 1)] [(0, 0), (1, 1)]) False
    equal (MSignal.pitches_share False 1 2 1 (from_pairs [(0, 1)]) 1
            (from_pairs [(1, 1)]))
        False
    -- Except when one note is in decay.
    equal (MSignal.pitches_share True 1 2 1 (from_pairs [(0, 1)]) 1
            (from_pairs [(1, 1)]))
        True

    -- Not an integral difference.
    equal (f 1 3 [(1, 60.01)] [(1, 61)]) False
    -- But this difference is inaudible.
    equal (f 1 3 [(1, 60.001)] [(1, 61)]) True

    -- Signals with different starting times.
    equal (f 1 3 [(1, 61), (10, 61)] [(0, 60), (10, 60)]) True

    -- one pitch changes but the other doesn't, so they can't share
    equal (f 0 3 [(0, 0), (1, 1)] [(0, 2)]) False
    equal (f 1 5 [(1, 74), (2, 76), (3, 74)] [(0, 48)]) False
