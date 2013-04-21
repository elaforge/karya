module App.SendCmd where
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Network
import qualified System.IO as IO
import qualified System.Posix as Posix

import qualified App.Config as Config


initialize :: IO a -> IO a
initialize app = Network.withSocketsDo $ do
    Posix.installHandler Posix.sigPIPE (Posix.Catch sigpipe) Nothing
    app
    where
    sigpipe = IO.hPutStrLn IO.stderr
        "caught SIGPIPE, reader must have closed the socket"


send :: String -> IO Text.Text
send msg = do
    hdl <- Network.connectTo "localhost" Config.repl_port
    IO.hPutStr hdl msg
    IO.hPutStr hdl Config.message_complete_token
    IO.hFlush hdl
    fmap Text.strip $ Text.IO.hGetContents hdl
