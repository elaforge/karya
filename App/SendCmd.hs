-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module App.SendCmd where
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Network
import qualified System.IO as IO
import qualified System.Posix as Posix

import qualified App.ReplUtil as ReplUtil


initialize :: IO a -> IO a
initialize app = Network.withSocketsDo $ do
    Posix.installHandler Posix.sigPIPE (Posix.Catch sigpipe) Nothing
    app
    where
    sigpipe = IO.hPutStrLn IO.stderr
        "caught SIGPIPE, reader must have closed the socket"

-- | I don't expect any newlines in the sent message.
send :: FilePath -> Text -> IO (Text, [Text]) -- ^ (response, logs)
send socket msg = do
    hdl <- Network.connectTo "localhost" (Network.UnixSocket socket)
    ByteString.Char8.hPutStr hdl $ ReplUtil.encode_request $
        Text.replace "\n" " " $ Text.strip msg
    IO.hFlush hdl
    ReplUtil.decode_response <$> ByteString.Char8.hGetContents hdl
