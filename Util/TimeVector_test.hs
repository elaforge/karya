-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.TimeVector_test where
import qualified Util.Lists as Lists
import Util.Test
import qualified Util.TimeVector as V
import Util.TimeVector (X)

import qualified Perform.RealTime as RealTime


type Y = Double

from_pairs :: [(X, Y)] -> V.Unboxed
from_pairs = V.from_pairs

to_pairs :: V.Unboxed -> [(X, Y)]
to_pairs = V.to_pairs

unsample :: V.Sample y -> (X, y)
unsample s = (V.sx s, V.sy s)

test_at :: Test
test_at = do
    let range low high sig = [V.at p (from_pairs sig)
            | p <- Lists.range low (high-1) 1] :: [Maybe Y]
    equal (range 0 4 []) (replicate 4 Nothing)
    equal (range 0 5 (zip (Lists.range_ 0 1) [0, 0.25, 0.5, 0.75, 1]))
        (map Just [0, 0.25, 0.5, 0.75, 1])

    -- Values before the first sample are Nothing.
    equal (range 0 4 [(2, 1)]) [Nothing, Nothing, Just 1, Just 1]
    equal (range 0 4 [(0, 1), (3, 1)]) (map Just [1, 1, 1, 1])

test_at_eta :: Test
test_at_eta = do
    let f x sig = V.at x (from_pairs sig)
    equal (f 6.666666666666664 [(6.666666666666666, 1)]) (Just 1)
    equal (f 6.6666666665 [(6.666666666666666, 1)]) Nothing

test_ascending :: Test
test_ascending = do
    let f p = map unsample $ V.ascending p (from_pairs [(1, 1), (2, 2), (3, 3)])
    equal (f 2) [(2, 2), (3, 3)]
    equal (f 3) [(3, 3)]
    equal (f 4) []

test_descending :: Test
test_descending = do
    let f p = map unsample $
            V.descending p (from_pairs [(1, 1), (2, 2), (3, 3)])
    equal (f 3) [(2, 2), (1, 1)]
    equal (f 2) [(1, 1)]
    equal (f 1) []

test_within :: Test
test_within = do
    let f s e = to_pairs . V.within s e . from_pairs
    equal (f 3 3 [(0, 0)]) [(0, 0)]
    equal (f 3 3 [(0, 0), (3, 3), (4, 4)]) [(3, 3)]

-- * transformation

test_shift :: Test
test_shift = do
    let vec = from_pairs [(0, 1), (1, 0)]
    let shift x = to_pairs $ V.shift x vec
    equal (shift 0) [(0, 1), (1, 0)]
    equal (shift 1) [(1, 1), (2, 0)]

test_drop_after :: Test
test_drop_after = do
    let f x = to_pairs . V.drop_at_after x . from_pairs
    let vec = [(0, 0), (1, 1), (2, 0)]
    equal (f 1 vec) [(0, 0)]
    equal (f 2 vec) [(0, 0), (1, 1)]
    equal (f 3 vec) [(0, 0), (1, 1), (2, 0)]
    equal (f 1 [(0, 0), (1 - RealTime.eta, 1)]) [(0, 0)]

test_drop_before :: Test
test_drop_before = do
    let f x = to_pairs . V.drop_before x . from_pairs
    let vec = [(2, 0), (4, 1)]
    equal (f 0 vec) [(2, 0), (4, 1)]
    equal (f 2 vec) [(2, 0), (4, 1)]
    equal (f 3 vec) [(2, 0), (4, 1)]
    equal (f 4 vec) [(4, 1)]
    equal (f 900 vec) [(4, 1)]
    equal (f 1 []) []
    equal (f 1 [(0, 0), (1, 0), (1, 1), (2, 1)]) [(1, 1), (2, 1)]

test_drop_before_strict :: Test
test_drop_before_strict = do
    let f x = to_pairs $ V.drop_before_strict x (from_pairs [(2, 0), (4, 1)])
    equal (f 0) [(2, 0), (4, 1)]
    equal (f 2) [(2, 0), (4, 1)]
    equal (f 3) [(4, 1)]
    equal (f 5) []
