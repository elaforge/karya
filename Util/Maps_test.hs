-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Maps_test where
import qualified Data.Map as Map
import qualified Util.Maps as Maps
import Util.Test


test_unique_unions = do
    let f = (\(m1, m2) -> (Map.toList m1, Map.toList m2))
            . Maps.unique_unions . map Map.fromList
    equal (f [[('a', 1)], [('a', 2)]])
        ([('a', 1)], [('a', 2)])
    equal (f [[('a', 1)], [('a', 2)], [('a', 3)]])
        ([('a', 1)], [('a', 2)])
    equal (f [[('a', 1), ('b', 2)], [('a', 2)]])
        ([('a', 1), ('b', 2)], [('a', 2)])

test_lookup_closest = do
    let f m k = snd <$> Maps.lookup_closest k m
    equal (map (f (Map.fromList [(1, 'a'), (5, 'b')])) [0..6])
        (map Just "aaaabbb")
    equal (map (f (Map.fromList [(1, 'a')])) [0..2]) (map Just "aaa")
