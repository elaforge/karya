{- Simple readline-using repl to talk to seq.
-}
import Control.Monad
import qualified Control.Exception as Exception
import qualified System.Console.Readline as Readline

import qualified App.SendCmd as SendCmd


main = SendCmd.initialize $ do
    putStrLn "^D to quit"
    while (Readline.readline "> ") handle_cmd ""
    putChar '\n'

while cond op state =
    maybe (return ()) (\x -> while cond op =<< op x state) =<< cond

handle_cmd msg prev = do
    when (not (null msg) && msg /= prev) $
        Readline.addHistory msg
    response <- SendCmd.send msg
        `Exception.catch` \exc -> return ("error: " ++ show exc)
    unless (null response) $
        putStrLn response
    return msg
