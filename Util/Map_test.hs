module Util.Map_test where
import qualified Data.Map as Map
import qualified Util.Map as Map
import Util.Test


test_unique_unions = do
    let f = (\(m1, m2) -> (Map.toList m1, Map.toList m2))
            . Map.unique_unions . map Map.fromList
    equal (f [[('a', 1)], [('a', 2)]])
        ([('a', 1)], [('a', 2)])
    equal (f [[('a', 1)], [('a', 2)], [('a', 3)]])
        ([('a', 1)], [('a', 2)])
    equal (f [[('a', 1), ('b', 2)], [('a', 2)]])
        ([('a', 1), ('b', 2)], [('a', 2)])
