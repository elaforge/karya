-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Format and display log msgs.
--
-- TODO: formatting options
module LogView.LogCat where
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text.IO as Text.IO
import qualified GHC.IO.Encoding as Encoding
import qualified System.Environment as Environment
import qualified System.IO as IO

import qualified Util.Log as Log
import qualified LogView.Tail as Tail

import           Control.Monad


main :: IO ()
main = do
    Encoding.setLocaleEncoding Encoding.utf8
    args <- Environment.getArgs
    hdl <- case args of
        [] -> return IO.stdin
        [fn] -> IO.openFile fn IO.ReadMode
        _ -> error "usage: logcat [filename]"
    IO.hSetBuffering hdl IO.LineBuffering
    forever $ do
        line <- ByteString.Char8.hGetLine hdl
        let msg = Tail.deserialize_line line
        Text.IO.putStrLn (Log.format_msg msg)
