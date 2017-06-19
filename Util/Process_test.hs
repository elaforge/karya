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
    tid <- Concurrent.forkIO $
        Process.supervised (System.Process.shell "sleep 1; echo sub done")
    -- With the kill, I shouldn't see "sub done".
    Thread.delay 0.1
    Concurrent.killThread tid

manual_test_supervised_no_binary =
    Process.supervised (System.Process.proc "aoeu" [])

manual_test_multiple_supervised = do
    let cmds = ["sleep 1; echo 1", "sleep 2; echo 2"]
    tid <- Concurrent.forkIO $
        Process.multipleSupervised (map System.Process.shell cmds)
    -- Should see two 'killing' messages.
    Thread.delay 0.1
    Concurrent.killThread tid
