-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Shake.Progress (report) where
import qualified Control.Concurrent as Concurrent
import qualified Development.Shake as Shake
import qualified System.Console.Regions as Regions
import qualified Text.Printf as Printf

import           Control.Monad


report :: IO Shake.Progress -> IO ()
report getProgress =
    void $ Concurrent.forkIO $ Regions.withConsoleRegion Regions.Linear loop
    where
    loop region = do
        progress <- getProgress
        Regions.setConsoleRegion region $ format progress
        Concurrent.threadDelay $ 1 * 10^6
        loop region
    format progress = Printf.printf
        (banner "skip %03d / built %03d / todo %03d -- %.1fs")
        (Shake.countSkipped progress) (Shake.countBuilt progress)
        (Shake.countTodo progress) (fst (Shake.timeTodo progress))
        ++ if snd (Shake.timeTodo progress) == 0 then ""
            else Printf.printf " (%d unknown)" (snd (Shake.timeTodo progress))
    banner msg = unwords [replicate 6 '=', msg, replicate 6 '=']
