-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Random where
import qualified System.Random.Shuffle as Shuffle

import qualified Util.Num as Num


-- | Shuffle a list by a list of random integers.  The random list must be
-- at least as long as the list to shuffle.
shuffle :: [a] -> [Int] -> [a]
shuffle xs = Shuffle.shuffle xs . mangle
    where
    -- Shuffle has a really complicated precondition and crashes if it doesn't
    -- get it.
    mangle rs = [Num.restrict 0 (len+1-i) v | (i, v) <- zip [0..] (take len rs)]
    len = length xs - 1
