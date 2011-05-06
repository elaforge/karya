module Util.Ranges_test where
import qualified Data.Monoid as Monoid

import Util.Test

import qualified Util.Ranges as Ranges


test_ranges = do
    let f :: [(Int, Int)] -> Maybe [(Int, Int)]
        f = Ranges.extract . Ranges.ranges
    equal (f []) (Just [])
    equal (f [(0, 0), (1, 1)]) (Just [(0, 0), (1, 1)])
    equal (f [(0, 1), (1, 2), (3, 4)]) (Just [(0, 2), (3, 4)])
    equal (f [(0, 3), (0, 2), (1, 1)]) (Just [(0, 3)])

test_merge = do
    let f r1 r2 = Ranges.extract
            (Monoid.mappend (Ranges.ranges r1) (Ranges.ranges r2))
    equal (f [(0, 0)] [(1, 1)]) (Just [(0, 0), (1, 1)])
    equal (f [(0, 0)] [(0, 1)]) (Just [(0, 1)])
    equal (f [(0, 1)] [(0, 0)]) (Just [(0, 1)])
    equal (f [(0, 1)] [(1, 2)]) (Just [(0, 2)])
    equal (f [(0, 2)] [(1, 3)]) (Just [(0, 3)])
    equal (f [(0, 1), (2, 3)] [(1, 2)]) (Just [(0, 3)])
