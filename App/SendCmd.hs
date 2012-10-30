module App.SendCmd where

import qualified Network
import qualified System.IO as IO
import qualified System.Posix as Posix

import qualified Util.Seq as Seq
import qualified App.Config as Config


initialize :: IO a -> IO a
initialize app = Network.withSocketsDo $ do
    Posix.installHandler Posix.sigPIPE (Posix.Catch sigpipe) Nothing
    app
    where
    sigpipe = IO.hPutStrLn IO.stderr
        "caught SIGPIPE, reader must have closed the socket"


send :: String -> IO String
send msg = do
    hdl <- Network.connectTo "localhost" Config.lang_port
    IO.hPutStr hdl msg
    IO.hPutStr hdl Config.message_complete_token
    IO.hFlush hdl
    fmap Seq.rstrip $ IO.hGetContents hdl
