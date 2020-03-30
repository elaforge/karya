-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.LazyVector_test where
import qualified Data.Vector as Vector

import qualified Util.LazyVector as LazyVector
import Util.Test


-- property: if you construct out of mixed singletons and fromLists, it will
-- always be the same as using list and (++).

test_builder = do
    let f = Vector.toList . LazyVector.toStrict . LazyVector.build
        chunks = map Vector.toList . LazyVector.lazyChunks . LazyVector.build
    let s = LazyVector.singleton
        list = LazyVector.fromList
    equal (f (s 1)) [1]
    equal (f (s 1 <> s 2)) [1, 2]
    equal (f (s 1 <> s 2 <> list [3, 4, 5])) [1..5]
    equal (f (s 1 <> s 2 <> list [3..1024])) [1..1024]
    equal (f ((mempty :: LazyVector.Builder Int) <> mempty)) []

    equal (chunks (mempty <> s 1 <> s 2 <> mempty)) [[1, 2]]
    equal (chunks (s 1 <> mempty <> s 2)) [[1, 2]]
