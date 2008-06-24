module Util.Thread where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception

import qualified Util.Log as Log

start_thread = do_start_thread Concurrent.forkIO
start_os_thread = do_start_thread Concurrent.forkOS

do_start_thread fork name th = fork $ Exception.bracket_
    (Log.notice $ "thread start: " ++ name)
    (Log.notice $ "thread exit: " ++ name)
    th
