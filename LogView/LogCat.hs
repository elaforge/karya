-- | Format and display log msgs.
--
-- TODO: formatting options
import Control.Monad
import qualified System.Environment as Environment
import qualified System.IO as IO

import qualified Util.Log as Log
import qualified LogView.Process as Process


main :: IO ()
main = do
    args <- Environment.getArgs
    hdl <- case args of
        [] -> return IO.stdin
        [fn] -> IO.openFile fn IO.ReadMode
        _ -> error $ "usage: logcat [filename]"
    IO.hSetBuffering hdl IO.LineBuffering
    forever $ do
        line <- IO.hGetLine hdl
        msg <- Process.deserialize_line line
        putStrLn (Log.format_msg msg)
