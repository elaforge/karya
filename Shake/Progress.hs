-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Shake.Progress (report) where
import qualified Control.Concurrent as Concurrent
import Control.Monad
import qualified Data.Maybe as Maybe
import qualified Development.Shake as Shake
import qualified Text.Printf as Printf


report :: IO Shake.Progress -> IO ()
report progress = void $ Concurrent.forkIO (loop "")
    where
    loop prev = do
        cur <- progress
        when (Shake.isRunning cur) $ do
            let msg = Maybe.fromMaybe "" (format cur)
            when (not (null msg) && msg /= prev) $
                putStrLn msg
            Concurrent.threadDelay $ 4 * 10^6
            loop msg
    format cur
        | Shake.countTodo cur == 0 = Nothing
        | otherwise = Just $ Printf.printf
            "====== skip %03d / built %03d / todo %03d -- %.1fs ======"
            (Shake.countSkipped cur) (Shake.countBuilt cur)
            (Shake.countTodo cur) (fst (Shake.timeTodo cur))
            ++ if snd (Shake.timeTodo cur) == 0 then ""
                else Printf.printf " (%d unknown)" (snd (Shake.timeTodo cur))
