module LogView.Timer where

import qualified Control.Concurrent.Chan as Chan
import Control.Monad
import qualified Data.Time as Time
import qualified System.Environment
import qualified System.IO as IO

import qualified Util.Thread as Thread
import qualified Util.Log as Log

import qualified LogView.Process as Process

delay_threshold = 0.01


main = do
    args <- System.Environment.getArgs
    hdl <- case args of
        [] -> return IO.stdin
        [filename] -> do
            hdl <- IO.openFile filename IO.ReadWriteMode
            IO.hSetBuffering hdl IO.LineBuffering
            IO.hSeek hdl IO.SeekFromEnd 0
            return hdl
        _ -> error $ "usage: timer [filename]"
    msgs <- Chan.newChan
    Thread.start_logged "tail_hdl" (tail_hdl msgs hdl)
    (last_date, _) <- Chan.readChan msgs
    print_loop msgs last_date

print_loop msgs last_date = do
    (date, msg) <- Chan.readChan msgs
    putStrLn $ show (date `Time.diffUTCTime` last_date) ++ "\t"
        ++ Log.msg_string msg
    print_loop msgs date

tail_hdl :: Chan.Chan (Time.UTCTime, Log.Msg) -> IO.Handle -> IO ()
tail_hdl msgs hdl = forever $ do
    line <- Process.tail_getline hdl
    msg <- Process.deserialize_line line
    case time_of msg of
        Just date -> Chan.writeChan msgs (date, msg)
        Nothing -> return ()

time_of msg@(Log.Msg { Log.msg_prio = Log.Timer, Log.msg_date = date }) =
    Just date
time_of _ = Nothing
