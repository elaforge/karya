{- Simple command-line tool to connect to the given socket and send some text,
and then receive the response.

It's used to talk over the seq_language socket.
-}
import Control.Monad
import qualified App.SendCmd as SendCmd
import qualified System.Environment as Environment
import qualified Util.Seq as Seq

main = SendCmd.initialize $ do
    args <- Environment.getArgs
    msg <- if null args
        then getContents
        else return $ Seq.join " " args ++ "\n"
    putStrLn $ "sending " ++ show msg
    response <- SendCmd.send msg
    unless (null response) $
        putStrLn response
