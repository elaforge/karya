-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Simple multiset implementation.
--
-- Yes, there's one on hackage, but it breaks on new ghcs.
module Util.MultiSet where
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map


type MultiSet k = Map.Map k Int

insert :: Ord k => k -> MultiSet k -> MultiSet k
insert key = Map.insertWith (+) key 1

lookup :: Ord k => k -> MultiSet k -> Int
lookup = Map.findWithDefault 0
