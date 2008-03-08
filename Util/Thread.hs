module Util.Thread where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception

start_thread = do_start_thread Concurrent.forkIO
start_os_thread = do_start_thread Concurrent.forkOS

do_start_thread fork name th = fork $ Exception.bracket_
    (putStrLn $ "thread start: " ++ name)
    (putStrLn $ "thread exit: " ++ name)
    th

