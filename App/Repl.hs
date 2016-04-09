-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
{- | Simple repl to talk to seq.
-}
module App.Repl where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Typeable as Typeable

import qualified System.Console.Haskeline as Haskeline
import qualified System.Console.Haskeline.MonadException
       as Haskeline.MonadException
import qualified System.FilePath as FilePath

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified LogView.Process as Process
import qualified LogView.Tail as Tail
import qualified App.ReplUtil as ReplUtil
import qualified App.SendCmd as SendCmd
import Global


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

type CurrentHistory = MVar.MVar (Maybe FilePath)

main :: IO ()
main = SendCmd.initialize $ do
    liftIO $ putStrLn "^D to quit"
    fname <- Tail.log_filename
    done <- MVar.newEmptyMVar
    current_history <- MVar.newMVar Nothing
    repl_thread <- Concurrent.forkIO $ do
        input_loop current_history initial_settings
        MVar.putMVar done ()
    hdl <- Tail.open fname (Just 0)
    Concurrent.forkIO $ loop repl_thread current_history hdl
    MVar.takeMVar done
    where
    loop repl_thread current_history = go
        where
        go hdl = do
            (msg, hdl) <- Tail.tail hdl
            whenJust (save_dir_of (Log.msg_text msg)) $ \dir -> do
                changed <- MVar.modifyMVar current_history $ \current ->
                    return (Just dir, current /= Just dir)
                when changed $
                    Concurrent.throwTo repl_thread SaveFileChanged
            go hdl

data SaveFileChanged = SaveFileChanged
    deriving (Show, Typeable.Typeable)
instance Exception.Exception SaveFileChanged

-- I have to modify history and read lines in the same thread.  But haskeline
-- blocks in getInputLine, and I don't think I can interrupt it.
input_loop :: CurrentHistory -> Haskeline.Settings IO -> IO ()
input_loop current_history settings = outer_loop settings
    where
    outer_loop settings = do
        x <- Haskeline.runInputT settings $ Haskeline.withInterrupt $
            repl (Haskeline.historyFile settings)
        whenJust x $ \maybe_fname -> do
            putStrLn $ "loading history from "
                ++ maybe "<no file>" show maybe_fname
            outer_loop $ settings { Haskeline.historyFile = maybe_fname }

    repl maybe_fname = run maybe_fname >>= \x -> case x of
        Continue -> repl maybe_fname
        Quit -> return Nothing
        Load -> liftIO $ Just <$> MVar.readMVar current_history
    run maybe_fname = Haskeline.MonadException.handle interrupt $
        Haskeline.MonadException.handle changed $
            ifM (read_eval_print =<< get_input maybe_fname)
                (return Continue) (return Quit)
    interrupt Haskeline.Interrupt = do
        Haskeline.outputStrLn "interrupted"
        return Continue
    changed SaveFileChanged = return Load

read_eval_print :: Maybe String -> Input Bool
read_eval_print Nothing = return False
read_eval_print (Just input)
    | null input = return True
    | otherwise = do
        response <- liftIO $ Exception.handle catch_all $
            ReplUtil.format_response <$> SendCmd.send (Text.pack input)
        unless (Text.null response) $
            liftIO $ Text.IO.putStrLn response
        return True
    where
    catch_all :: Exception.SomeException -> IO Text.Text
    catch_all exc = return $ "error: " <> Text.pack (show exc)

get_input :: Maybe FilePath -> Input (Maybe String)
get_input maybe_fname =
    fmap Seq.strip <$> Haskeline.getInputLine (prompt maybe_fname)

data Status = Continue | Quit | Load deriving (Show)

save_dir_of :: Text -> Maybe FilePath
save_dir_of msg =
    flip FilePath.replaceExtension "repl" . untxt <$> Map.lookup "save" status
    where status = Process.match_pattern Process.global_status_pattern msg

-- | Colorize the prompt to make it stand out.
prompt :: Maybe FilePath -> String
prompt maybe_fname = fname ++ cyan_bg ++ "入" ++ plain_bg ++ " "
    where
    fname = maybe "" (fst . Seq.drop_suffix ".repl" . FilePath.takeFileName)
        maybe_fname

-- The trailing \STX tells haskeline this is a control sequence, from
-- http://trac.haskell.org/haskeline/wiki/ControlSequencesInPrompt
cyan_bg :: String
cyan_bg = "\ESC[46m\STX"

plain_bg :: String
plain_bg = "\ESC[39;49m\STX"
