-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Measure the amount of memory taken by various operations.
module Cmd.Space_profile where
import qualified Data.Map as Map

import qualified Util.Memory as Memory
import Util.Test
import qualified Ui.State as State
import qualified Ui.Transform as Transform
import qualified Cmd.Save as Save


profile_load = do
    -- Check memory usage for just the state data.
    start_mem <- Memory.memory_usage
    state <- load "save/bloom"
    let (state2, table) = Transform.intern_text state
    putStrLn $ "loaded blocks: " ++ show (Map.size (State.state_blocks state))
    print_memory_diff start_mem =<< Memory.memory_usage
    let (saved, hits) = Transform.intern_stats table
    putStrLn $ "hits: " ++ show hits ++ " saved: " ++ show saved

-- Check memory usage loading a state with all the Responder machinery
-- (includes the cache).

-- Check memory usage after a bunch of changes.

load :: FilePath -> IO State.State
load fname = do
    result <- print_timer ("unserialize " ++ show fname)
        (const "") (Save.read_state fname)
    Just state <- return $ expect_right "load" result
    return state

print_memory_diff :: (Memory.Size, Memory.Size) -> (Memory.Size, Memory.Size)
    -> IO ()
print_memory_diff (rss1, vsz1) (rss2, vsz2) = do
    putStrLn $ "RSS: " ++ show rss1 ++ " -> " ++ show rss2 ++ " = "
        ++ show (rss2 - rss1)
    putStrLn $ "VSZ: " ++ show vsz1 ++ " -> " ++ show vsz2 ++ " = "
        ++ show (vsz2 - vsz1)
