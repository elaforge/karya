-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Signal2_test where
import qualified Util.Segment as Segment
import qualified Util.Seq as Seq
import Util.Test

import qualified Perform.Signal2 as Signal
import Perform.Signal2 (X, Y, from_pairs, to_pairs)


test_at = do
    let f sig x = Signal.at x (from_pairs sig)
    equal (map (f [(2, 2), (4, 0)]) (Seq.range 0 5 1)) [0, 0, 2, 1, 0, 0]
    equal (map (f [(2, 0), (4, 2)]) (Seq.range 0 5 1)) [0, 0, 0, 1, 2, 2]
    equal (f [(0, 2), (2, 0)] (-1)) 0
    equal (f [(-1, 2), (2, 0)] (-2)) 0

test_scale = do
    let f = Signal.scale 0.5
    equal (map f [-1, -0.5, 0, 0.5, 1]) [0, 0.25, 0.5, 0.75, 1]
    let invert = Signal.scale 0.5 . Signal.scale_invert 0.5
    equal (map invert [-1, -0.5, 0, 0.5, 1]) [-1, -0.5, 0, 0.5, 1]

-- test_pitches_share = do
--     let sample0 sig = fromIntegral (floor y)
--             where Just (_, y) = Signal.head sig
--     let f start end sig0 sig1 = Signal.pitches_share False start end
--             (sample0 sig0) sig0 (sample0 sig1) sig1
--
--     -- Different signals.
--     equal (f 0 1 (signal [(0, 0), (1, 1)]) (signal [(0, 1), (1, 0)])) False
--     -- Separated by an integer can share.
--     equal (f 0 1 (signal [(0, 0), (1, 1)]) (signal [(0, 1), (1, 2)])) True
--     -- Separated by 0 can't share.
--     equal (f 0 1 (signal [(0, 0), (1, 1)]) (signal [(0, 0), (1, 1)])) False
--     equal (Signal.pitches_share False 1 2 1 (signal [(0, 1)]) 1
--             (signal [(1, 1)]))
--         False
--     -- Except when one note is in decay.
--     equal (Signal.pitches_share True 1 2 1 (signal [(0, 1)]) 1
--             (signal [(1, 1)]))
--         True
--
--     -- Not an integral difference.
--     equal (f 1 3 (signal [(1, 60.01)]) (signal [(1, 61)])) False
--     -- But this difference is inaudible.
--     equal (f 1 3 (signal [(1, 60.001)]) (signal [(1, 61)])) True
--
--     -- Signals with different starting times.
--     equal (f 1 3 (signal [(1, 61), (10, 61)]) (signal [(0, 60), (10, 60)]))
--         True
--
--     -- one pitch changes but the other doesn't, so they can't share
--     equal (f 0 3 (signal [(0, 0), (1, 1)]) (signal [(0, 2)])) False
--     equal (f 1 5 (signal [(1, 74), (2, 76), (3, 74)]) (signal [(0, 48)])) False

test_scalar_max = do
    let f v = to_pairs . Signal.scalar_max v . from_pairs
    equal (f 1 [(1, 1), (4, 4)]) [(1, 1), (4, 4)]
    equal (f 1 [(0, 0), (4, 4)]) [(0, 1), (1, 1), (4, 4)]
    equal (f 1 [(0, 0), (0, 0), (4, 4)]) [(0, 1), (1, 1), (4, 4)]
    equal (f 1 [(0, 0), (4, 4), (8, 0)]) [(0, 1), (1, 1), (4, 4), (7, 1)]
    equal (f 1 [(0, 4), (4, 0)]) [(0, 4), (3, 1)]
    equal (f 1 [(0, 4), (4, 0), (5, 0.5)]) [(0, 4), (3, 1)]
    equal (f 1 [(0, 4), (4, 0), (8, 4)]) [(0, 4), (3, 1), (5, 1), (8, 4)]

-- * utilities

to_segments :: Signal.Signal kind -> [((X, Y), (X, Y))]
to_segments = map (\(Segment.Segment x1 y1 x2 y2) -> ((x1, y1), (x2, y2)))
    . Signal.to_segments
