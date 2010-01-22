{- | Simple repl to talk to seq.
-}
import Control.Monad
import Control.Monad.Trans (liftIO)
import qualified Control.Exception as Exception
import qualified System.Console.Haskeline as Haskeline
import qualified Util.Seq as Seq

import qualified App.SendCmd as SendCmd


settings = Haskeline.defaultSettings
    { Haskeline.historyFile = Just "repl.history"
    , Haskeline.autoAddHistory = True
    }

main :: IO ()
main = SendCmd.initialize $ Haskeline.runInputT settings $ do
    liftIO $ putStrLn "^D to quit"
    repl

repl :: Haskeline.InputT IO ()
repl = do
    maybe_line <- Haskeline.getInputLine "> "
    case maybe_line of
        Just line
            | null (Seq.strip line) -> repl
            | otherwise -> do
                response <- liftIO $ SendCmd.send line
                    `Exception.catch` catch_all
                unless (null response) $
                    liftIO $ putStrLn response
                repl
        Nothing -> return ()

catch_all :: Exception.SomeException -> IO String
catch_all exc = return ("error: " ++ show exc)
