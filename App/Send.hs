{- Simple command-line tool to connect to the given socket and send some text,
and then receive the response.

It's used to talk over the seq_language socket.

TODO response not implemented yet, responses go to app stdout
-}
import qualified Network
import qualified System.IO as IO
import qualified System.Posix as Posix

import qualified App.Config as Config


main = Network.withSocketsDo $ do
    Posix.installHandler Posix.sigPIPE (Posix.Catch sigpipe) Nothing
    hdl <- Network.connectTo "localhost" Config.lang_port
    s <- IO.getContents
    IO.hPutStr hdl s
    IO.hClose hdl

sigpipe = do
    IO.hPutStrLn IO.stderr "caught SIGPIPE, reader must have closed the socket"
