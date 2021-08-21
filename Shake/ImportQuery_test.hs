-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Shake.ImportQuery_test where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import qualified Shake.ImportQuery as ImportQuery

import           Global
import           Util.Test


test_addDep :: Test
test_addDep = do
    let f = ImportQuery.addDep g0
    equal (f "A" "Z") $
        Left [["A", "B", "Y", "Z"], ["A", "C", "Y", "Z"]]
    equal (f "A" "BB") $ Right $
        Tree.Node "BB" [Tree.Node "CC" [], Tree.Node "C*" []]

test_rmDep :: Test
test_rmDep = do
    let f parent = map (second Set.toList) . ImportQuery.rmDep cached parent
    pprint (f "A" "D")
    equal (f "A" "B") [("A", ["B"])]
    pprint (f "C" "Y")

cached :: ImportQuery.CachedGraph
cached = ImportQuery.cachedGraph g0

g0 :: ImportQuery.Graph
g0 = Map.fromList
    [ ("A", ["B", "C", "D"])
    , ("B", ["Y"])
    , ("C", ["Y"])
    , ("Y", ["Z"])
    , ("AA", ["BB"])
    , ("BB", ["CC", "C"])
    ]
