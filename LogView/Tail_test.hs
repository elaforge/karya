-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module LogView.Tail_test where
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified System.IO as IO

import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified LogView.Tail as Tail

import           Global


-- TODO
-- The log tail thread sometimes crashes with
-- Posix.getFileStatus: gets openFile: resource busy (file is locked)
-- Presumably this has to do with how the ghc IO manager locks files when
-- opened, but I can't reproduce the problem.  If the problem comes up again I
-- can start with this to try to reproduce.

reader :: IO ()
reader = do
    hdl <- Tail.open log_fn Nothing
    go hdl
    where
    go hdl = do
        (msg, hdl) <- Tail.tail hdl
        print msg
        go hdl

writer :: IO ()
writer = forM_ (map showt [0..]) $ \n -> do
    hdl <- Tail.rotate_logs 2 400 log_fn
    write hdl n
    Thread.delay 2
    IO.hClose hdl

log_fn :: FilePath
log_fn = "build/test.log"

write :: IO.Handle -> Text -> IO ()
write hdl msg = do
    ByteString.Lazy.hPutStr hdl $ Log.serialize (Log.msg Log.Notice Nothing msg)
    ByteString.Lazy.hPutStr hdl "\n"
