-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Signal_test where
import qualified Util.Segment as Segment
import qualified Util.Seq as Seq
import Util.Test

import qualified Perform.Signal as Signal
import Perform.Signal (X, Y, from_pairs, to_pairs)


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
