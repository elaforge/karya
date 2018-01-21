-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.TimeVector_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.TimeVector as V
import Util.TimeVector (X)

import qualified Perform.RealTime as RealTime
import Global


type Y = Double

from_pairs :: [(X, Y)] -> V.Unboxed
from_pairs = V.from_pairs

to_pairs :: V.Unboxed -> [(X, Y)]
to_pairs = V.to_pairs

unsample :: V.Sample y -> (X, y)
unsample s = (V.sx s, V.sy s)

test_at = do
    let range low high sig = [V.at p (from_pairs sig)
            | p <- Seq.range low (high-1) 1] :: [Maybe Y]
    equal (range 0 4 []) (replicate 4 Nothing)
    equal (range 0 5 (zip (Seq.range_ 0 1) [0, 0.25, 0.5, 0.75, 1]))
        (map Just [0, 0.25, 0.5, 0.75, 1])

    -- Values before the first sample are Nothing.
    equal (range 0 4 [(2, 1)]) [Nothing, Nothing, Just 1, Just 1]
    equal (range 0 4 [(0, 1), (3, 1)]) (map Just [1, 1, 1, 1])

test_at_before = do
    let sig = from_pairs [(1, 1), (1, 2), (3, 3)]
    equal (map (flip V.at sig) (Seq.range' 0 5 1))
        [Nothing, Just 2, Just 2, Just 3, Just 3]
    equal (map (flip V.sample_at_before sig) (Seq.range' 0 5 1))
        [Nothing, Just (1, 1), Just (1, 2), Just (3, 3), Just (3, 3)]

test_at_eta = do
    let f x sig = V.at x (from_pairs sig)
    equal (f 6.666666666666664 [(6.666666666666666, 1)]) (Just 1)
    equal (f 6.6666666665 [(6.666666666666666, 1)]) Nothing

test_ascending = do
    let f p = map unsample $ V.ascending p (from_pairs [(1, 1), (2, 2), (3, 3)])
    equal (f 2) [(2, 2), (3, 3)]
    equal (f 3) [(3, 3)]
    equal (f 4) []

test_descending = do
    let f p = map unsample $
            V.descending p (from_pairs [(1, 1), (2, 2), (3, 3)])
    equal (f 3) [(2, 2), (1, 1)]
    equal (f 2) [(1, 1)]
    equal (f 1) []

test_within = do
    let f s e = to_pairs . V.within s e . from_pairs
    equal (f 3 3 [(0, 0)]) [(0, 0)]
    equal (f 3 3 [(0, 0), (3, 3), (4, 4)]) [(3, 3)]

test_bsearch = do
    let run f = map (flip f sig) [0, 1, 2, 3, 4]
        sig = from_pairs [(1, 1), (2, 2), (2, 3), (3, 3)]
    equal (run V.bsearch_above) [ 0, 1, 3, 4, 4]
    equal (run V.highest_index) [-1, 0, 2, 3, 3]
    equal (run V.bsearch_below)    [ 0, 0, 1, 3, 4]
    equal (run V.bsearch_below_1)  [ 0, 1, 2, 4, 4]
    equal (run V.index_below)  [-1, 0, 0, 2, 3]

-- * transformation

test_merge_right_extend = do
    let f = to_pairs . V.merge_right_extend . map from_pairs
    equal (f [[(0, 1)], [(4, 2)]]) [(0, 1), (4, 1), (4, 2)]
    equal (f [[(0, 1), (1, 2)], [(1, 4)]]) [(0, 1), (1, 2), (1, 4)]
    -- The rightmost one wins.
    equal (f [[(0, 0)], [(2, 2)], [(1, 1)]]) [(0, 0), (1, 0), (1, 1)]

test_merge_right = do
    let f = to_pairs . V.merge_right . map from_pairs
    equal (f []) []
    equal (f [[(0, 0), (1, 1)]]) [(0, 0), (1, 1)]
    equal (f [[(1, 1)], [(2, 2)], [(3, 3)]])
        [(1, 1), (2, 2), (3, 3)]
    equal (f [[(0, 0), (1, 1), (2, 2)], [(1, 3), (2, 4), (3, 5)]])
        [(0, 0), (1, 3), (2, 4), (3, 5)]
    equal (f [[(0, 0), (1, 1)], [(1, 1), (2, 2)]])
        [(0, 0), (1, 1), (2, 2)]
    equal (f [[(0, 0), (1, 0)], [], [(1, 3)]])
        [(0, 0), (1, 3)]
    equal (f [[(0, 0), (1, 1)], [(1, 2)], [(1, 3), (2, 4)]])
        [(0, 0), (1, 3), (2, 4)]
    -- |--->        => |-|
    --   |--->           |-|
    --     |--->           |--->
    equal (f [[(0, 1), (4, 1)], [(2, 2), (6, 2)], [(4, 3), (8, 3)]])
        [(0, 1), (2, 2), (4, 3), (8, 3)]
    -- |--->           |
    --   |--->         |
    -- |--->           |--->
    equal (f [[(0, 1), (4, 1)], [(2, 2), (6, 2)], [(0, 3), (4, 3)]])
        [(0, 3), (4, 3)]

test_merge_left = do
    let f = to_pairs . V.merge_left . map from_pairs
    equal (f []) []
    equal (f [[(0, 0), (1, 1)]]) [(0, 0), (1, 1)]
    equal (f [[(1, 1)], [(2, 2)], [(3, 3)]])
        [(1, 1), (2, 2), (3, 3)]
    -- |--->        => |--->
    --   |--->             |->
    --     |--->             |->
    equal (f [[(0, 1), (4, 1)], [(2, 2), (6, 2)], [(4, 3), (8, 3)]])
        [(0, 1), (4, 1), (6, 2), (8, 3)]
    -- |--->           |--->
    --   |--->             |->
    -- |--->               |
    equal (f [[(0, 1), (4, 1)], [(2, 2), (6, 2)], [(0, 3), (4, 3)]])
        [(0, 1), (4, 1), (6, 2)]

test_prepend = do
    let f v1 v2 = to_pairs $ V.prepend (from_pairs v1) (from_pairs v2)
    equal (f [] []) []
    equal (f [(0, 0), (1, 1)] [(0, 2), (1, 3), (2, 4)])
        [(0, 0), (1, 1), (2, 4)]

test_shift = do
    let vec = from_pairs [(0, 1), (1, 0)]
    let shift x = to_pairs $ V.shift x vec
    equal (shift 0) [(0, 1), (1, 0)]
    equal (shift 1) [(1, 1), (2, 0)]

test_drop_after = do
    let f x = to_pairs . V.drop_at_after x . from_pairs
    let vec = [(0, 0), (1, 1), (2, 0)]
    equal (f 1 vec) [(0, 0)]
    equal (f 2 vec) [(0, 0), (1, 1)]
    equal (f 3 vec) [(0, 0), (1, 1), (2, 0)]
    equal (f 1 [(0, 0), (1 - RealTime.eta, 1)]) [(0, 0)]

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

test_clip_to = do
    let f x = to_pairs . V.clip_to x . from_pairs
    equal (f 1 [(0, 0), (1, 1), (2, 2)]) [(1, 1), (2, 2)]
    equal (f 1.5 [(0, 0), (1, 1), (2, 2)]) [(1.5, 1), (2, 2)]

test_drop_before_strict = do
    let f x = to_pairs $ V.drop_before_strict x (from_pairs [(2, 0), (4, 1)])
    equal (f 0) [(2, 0), (4, 1)]
    equal (f 2) [(2, 0), (4, 1)]
    equal (f 3) [(4, 1)]
    equal (f 5) []

test_sig_op = do
    let f vec0 vec1 = to_pairs $
            V.sig_op 0 (+) (from_pairs vec0) (from_pairs vec1)
    equal (f [(1, 1)] []) [(1, 1)]
    equal (f [] [(1, 1)]) [(1, 1)]
    equal (f [(0, 0), (2, 2), (4, 0)] [(0, 1)])
        [(0, 1), (2, 3), (4, 1)]
    equal (f [(0, 0), (2, 2), (4, 0)] [(1, 1), (3, 0)])
        [(0, 0), (1, 1), (2, 3), (3, 2), (4, 0)]

test_concat_map_accum = do
    let go accum x0 y0 x1 y1 = (accum+1, [V.Sample x0 y0, V.Sample x1 y1])
        final accum (V.Sample x y) = [V.Sample (x*10) (y*10+accum)]
        f vec = to_pairs (V.concat_map_accum 0 go final 0 (from_pairs vec))
    equal (f []) []
    equal (f [(0, 0), (1, 1), (2, 2)])
        [ (0, 0), (0, 0)
        , (0, 0), (1, 1)
        , (1, 1), (2, 2)
        , (20, 23)
        ]

test_strip = do
    let f = to_pairs . V.strip . from_pairs
    equal (f [(0, 0), (1, 0), (1, 1), (2, 1)]) [(0, 0), (1, 0), (1, 1), (2, 1)]
    equal (f [(0, 0), (1, 1), (1, 1), (2, 1)]) [(0, 0), (1, 1), (2, 1)]
    equal (f [(0, 0), (1, 1), (1, 2), (1, 1), (2, 1)]) [(0, 0), (1, 1), (2, 1)]


-- * signal-discontinuity

test_merge_segments = do
    let f = to_pairs . V.merge_segments . map (second from_pairs)
    equal (f [(0, [(0, 1)]), (1, [(0, 2)])]) [(0, 1), (1, 1), (1, 2)]
