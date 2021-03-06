{-# LANGUAGE StandaloneDeriving #-}
-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | It's like they worked hard to make System.Posix.Resource this clunky.
module Util.Limit (set, Resource(..)) where
import           System.Posix.Resource


deriving instance Show ResourceLimit
deriving instance Show ResourceLimits

set :: Resource -> Integer -> IO ()
set resource soft = do
    old <- getResourceLimit resource
    setResourceLimit resource $ ResourceLimits
        { softLimit = ResourceLimit soft
        , hardLimit = hardLimit old
        }
