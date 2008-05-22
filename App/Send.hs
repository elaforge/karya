{- Simple command-line tool to connect to the given socket and send some text,
and then receive the response.

It's used to talk over the seq_language socket.

TODO response not implemented yet, responses go to app stdout
-}
import Control.Monad
import qualified Network
import qualified System.IO as IO
import qualified System.Posix as Posix

import qualified Util.Seq as Seq
import qualified App.Config as Config


main = Network.withSocketsDo $ do
    Posix.installHandler Posix.sigPIPE (Posix.Catch sigpipe) Nothing
    hdl <- Network.connectTo "localhost" Config.lang_port
    IO.hSetBuffering hdl IO.NoBuffering
    s <- IO.getContents
    IO.hPutStr hdl s
    IO.hPutStr hdl Config.message_complete_token
    response <- fmap Seq.strip $ IO.hGetContents hdl
    when (not (null response)) $
        putStrLn response

sigpipe =
    IO.hPutStrLn IO.stderr "caught SIGPIPE, reader must have closed the socket"
