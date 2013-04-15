{- | Simple repl to talk to seq.
-}
module App.Repl where
import qualified Control.Exception as Exception
import Control.Monad
import Control.Monad.Trans (liftIO)

import qualified System.Console.Haskeline as Haskeline
import qualified System.Console.Terminfo as Terminfo

import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq
import qualified App.SendCmd as SendCmd


settings :: Haskeline.Settings IO
settings = Haskeline.defaultSettings
    { Haskeline.historyFile = Just "repl.history"
    , Haskeline.autoAddHistory = True
    }

main :: IO ()
main = SendCmd.initialize $ Haskeline.runInputT settings $ do
    liftIO $ putStrLn "^D to quit"
    term <- liftIO $ Terminfo.setupTermFromEnv
    Haskeline.withInterrupt $ while $
        Haskeline.handleInterrupt
            (Haskeline.outputStrLn "interrupted" >> return True)
            (repl term)

-- The trailing \STX tells haskeline this is a control sequence, from
-- http://trac.haskell.org/haskeline/wiki/ControlSequencesInPrompt
cyan_bg :: String
cyan_bg = "\ESC[46m\STX"

plain_bg :: String
plain_bg = "\ESC[39;49m\STX"

with_bg :: Terminfo.Terminal -> Terminfo.Color -> String -> String
with_bg term color s =
    case Terminfo.getCapability term Terminfo.withBackgroundColor of
        Just with -> with color s
        Nothing -> s

repl :: Terminfo.Terminal -> Haskeline.InputT IO Bool
repl term =
    -- Colorize the prompt to make it stand out.
    maybe (return False) ((>> return True) . handle)
        =<< Haskeline.getInputLine (cyan_bg ++ "入 " ++ plain_bg)
    where
    handle line
        | null (Seq.strip line) = return ()
        | otherwise = do
            response <- liftIO $ SendCmd.send line `Exception.catch` catch_all
            let formatted = Seq.strip $ format_response response
            unless (null formatted) $
                liftIO $ putStrLn formatted

format_response :: String -> String
format_response "()" = ""
format_response ('!':s) = s -- See 'Cmd.Lang.unformatted'.
format_response s = PPrint.format_str s

while :: (Monad m) => m Bool -> m ()
while action = do
    b <- action
    if b then while action else return ()

catch_all :: Exception.SomeException -> IO String
catch_all exc = return ("error: " ++ show exc)
