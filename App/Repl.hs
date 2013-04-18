{-# LANGUAGE DeriveDataTypeable #-}
{- | Simple repl to talk to seq.
-}
module App.Repl where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception

import qualified Data.Map as Map
import qualified Data.Typeable as Typeable
import qualified System.Console.Haskeline as Haskeline
import qualified System.Console.Haskeline.MonadException
       as Haskeline.MonadException
import qualified System.FilePath as FilePath

import Util.Control
import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq

import qualified LogView.Process as Process
import qualified LogView.Tail as Tail
import qualified App.SendCmd as SendCmd


type Input a = Haskeline.InputT IO a

initial_settings :: Haskeline.Settings IO
initial_settings = Haskeline.defaultSettings
    { Haskeline.historyFile = Nothing
    , Haskeline.autoAddHistory = True
    }

-- Getting the REPL to read a new history when the save file changes was more
-- of a hassle than I expected.  A separate thread tails the log file.  When it
-- sees a log line indicating a new save file, it throws an exception to the
-- REPL thread, which is otherwise blocked on user input.  When the REPL thread
-- gets the exception, it restarts runInputT with a new historyFile.

main :: IO ()
main = SendCmd.initialize $ do
    liftIO $ putStrLn "^D to quit"
    fname <- Tail.log_filename
    done <- MVar.newEmptyMVar
    repl_thread <- Concurrent.forkIO $ do
        repl initial_settings
        MVar.putMVar done ()
    hdl <- Tail.open fname (Just 0)
    Concurrent.forkIO $ loop repl_thread hdl
    MVar.takeMVar done
    where
    loop repl_thread hdl = do
        (msg, hdl) <- Tail.tail hdl
        when_just (save_dir_of (Log.msg_string msg)) $ \dir ->
            Concurrent.throwTo repl_thread (SaveFileChanged dir)
        loop repl_thread hdl

newtype SaveFileChanged = SaveFileChanged FilePath
    deriving (Show, Typeable.Typeable)
instance Exception.Exception SaveFileChanged

-- I have to modify history and read lines in the same thread.  But haskeline
-- blocks in getInputLine, and I don't think I can interrupt it.
repl :: Haskeline.Settings IO -> IO ()
repl settings = input_loop settings $
    maybe (return False) ((>> return True) . handle . Seq.strip)
        =<< Haskeline.getInputLine prompt
    where
    handle line
        | null line = return ()
        | otherwise = do
            response <- liftIO $ Exception.handle catch_all $ SendCmd.send line
            let formatted = Seq.strip $ format_response response
            unless (null formatted) $
                liftIO $ putStrLn formatted
    catch_all :: Exception.SomeException -> IO String
    catch_all exc = return ("error: " ++ show exc)

format_response :: String -> String
format_response "()" = ""
format_response ('!':s) = s -- See 'Cmd.Repl.unformatted'.
format_response s = PPrint.format_str s

input_loop :: Haskeline.Settings IO -> Input Bool -> IO ()
input_loop settings action = do
    x <- Haskeline.runInputT settings (Haskeline.withInterrupt loop)
    when_just x $ \fname -> do
        putStrLn $ "loading history from " ++ show fname
        input_loop (settings { Haskeline.historyFile = Just fname }) action
    where
    loop = run_action >>= \x -> case x of
        Continue -> loop
        Quit -> return Nothing
        Load dir -> return (Just dir)
    run_action = Haskeline.MonadException.handle interrupt $
        Haskeline.MonadException.handle changed $
            ifM action (return Continue) (return Quit)
    interrupt Haskeline.Interrupt = do
        Haskeline.outputStrLn "interrupted"
        return Continue
    changed (SaveFileChanged dir) = return $ Load dir

data Status = Continue | Quit | Load !FilePath

save_dir_of :: String -> Maybe FilePath
save_dir_of msg =
    flip FilePath.replaceExtension "repl" <$> Map.lookup "save" status
    where status = Process.match_pattern Process.global_status_pattern msg

-- | Colorize the prompt to make it stand out.
prompt :: String
prompt = cyan_bg ++ "入" ++ plain_bg ++ " "

-- The trailing \STX tells haskeline this is a control sequence, from
-- http://trac.haskell.org/haskeline/wiki/ControlSequencesInPrompt
cyan_bg :: String
cyan_bg = "\ESC[46m\STX"

plain_bg :: String
plain_bg = "\ESC[39;49m\STX"
