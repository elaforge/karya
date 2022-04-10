-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Ranges_test where
import qualified Data.Monoid as Monoid

import Util.Test

import qualified Util.Ranges as Ranges


test_ranges :: Test
test_ranges = do
    let f :: [(Int, Int)] -> Maybe [(Int, Int)]
        f = Ranges.extract . Ranges.ranges
    equal (f []) (Just [])
    equal (f [(0, 0), (1, 1)]) (Just [(0, 0), (1, 1)])
    equal (f [(0, 1), (1, 2), (3, 4)]) (Just [(0, 2), (3, 4)])
    equal (f [(0, 3), (0, 2), (1, 1)]) (Just [(0, 3)])

test_merge :: Test
test_merge = do
    let f r1 r2 = Ranges.extract
            (Monoid.mappend (Ranges.ranges r1) (Ranges.ranges r2))
    equal (f [(0, 0)] [(1, 1)]) (Just [(0, 0), (1, 1)])
    equal (f [(0, 0)] [(0, 1)]) (Just [(0, 1)])
    equal (f [(0, 1)] [(0, 0)]) (Just [(0, 1)])
    equal (f [(0, 1)] [(1, 2)]) (Just [(0, 2)])
    equal (f [(0, 2)] [(1, 3)]) (Just [(0, 3)])
    equal (f [(0, 1), (2, 3)] [(1, 2)]) (Just [(0, 3)])

test_invert :: Test
test_invert = do
    let f extent = Ranges.extract . Ranges.invert extent . Ranges.ranges
    equal (f (0, 5) [(1, 3)]) $ Just [(0, 1), (3, 5)]
    equal (f (0, 5) [(0, 3)]) $ Just [(3, 5)]
    equal (f (0, 5) [(0, 5)]) $ Just []
    equal (f (0, 5) [(1, 2), (2, 3)]) $ Just [(0, 1), (3, 5)]

test_overlapping :: Test
test_overlapping = do
    let closed r1 r2 = Ranges.overlapping_closed
            (Ranges.ranges r1) (Ranges.ranges r2)
        open r1 r2 = Ranges.overlapping (Ranges.ranges r1) (Ranges.ranges r2)
    equal (closed [(0, 1)] [(1, 2)]) True
    equal (open [(0, 1)] [(1, 2)]) False
