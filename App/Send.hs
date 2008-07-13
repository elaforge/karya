{- Simple command-line tool to connect to the given socket and send some text,
and then receive the response.

It's used to talk over the seq_language socket.
-}
import Control.Monad
import qualified App.SendCmd as SendCmd

main = SendCmd.initialize $ do
    msg <- getContents
    response <- SendCmd.send msg
    unless (null response) $
        putStrLn response
