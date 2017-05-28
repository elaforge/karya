-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Process_test where
import qualified Control.Concurrent as Concurrent
import qualified System.Process

import qualified Util.Process as Process
import qualified Util.Thread as Thread


-- It's kind of annoying to test this automatically, so just make sure it looks
-- right when run by hand.
manual_test_supervised = do
    let f cmd = Process.supervised (System.Process.shell cmd)
    tid <- Concurrent.forkIO $ f "sleep 1; echo sub done" $ \_ -> do
        Thread.delay 1
        putStrLn "thread done"
    -- With the kill, I shouldn't see "sub done".
    Thread.delay 0.1
    Concurrent.killThread tid
    putStrLn "done"
    Thread.delay 0.1

manual_test_multiple_supervised = do
    let f cmds = Process.multiple_supervised (map System.Process.shell cmds)
    f ["sleep 1; echo 1", "sleep 2; echo 2"] $ \hdls -> do
        putStrLn $ "hdls: " ++ show (length hdls)
    Thread.delay 3
