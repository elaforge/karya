-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase #-}
-- | Simple repl to talk to seq.
module App.Repl where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Console.Haskeline as Haskeline
import qualified System.Environment
import qualified System.FilePath as FilePath

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified LogView.Process as Process
import qualified LogView.Tail as Tail
import qualified App.Config as Config
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
-- sees a log line indicating a new save file, it puts it in an exception and
-- throws it to the REPL thread, which is otherwise blocked on user input.
-- When the REPL thread gets the exception, it restarts runInputT with the new
-- historyFile.

type CurrentHistory = MVar.MVar (Maybe FilePath)

main :: IO ()
main = SendCmd.initialize $ do
    args <- System.Environment.getArgs
    socket <- case args of
        [] -> return Config.repl_port
        [fn] -> return fn
        _ -> errorIO $ "usage: repl [ unix-socket ]"
    -- I don't want to see "thread started" logs.
    Log.configure $ \state -> state { Log.state_log_level = Log.Notice }
    liftIO $ putStrLn "^D to quit"
    log_fname <- Tail.log_filename
    done <- MVar.newEmptyMVar
    repl_thread <- Thread.start_logged "repl" $ do
        repl socket initial_settings
        MVar.putMVar done ()
    Thread.start_logged "watch_log" $
        watch_log repl_thread Nothing =<< Tail.open log_fname (Just 0)
    MVar.takeMVar done
    Thread.delay 0.15 -- give threads time to print an exit msg

-- | Watch the log handle.  When it indicates a new save dir, put it in
-- the given MVar and throw a 'SaveFileChanged' to the repl thread to
-- interrupt it.
watch_log :: Concurrent.ThreadId -> Maybe FilePath -> Tail.Handle -> IO ()
watch_log repl_thread = go
    where
    go current hdl = do
        (msg, hdl) <- Tail.tail hdl
        case save_dir_of (Log.msg_text msg) of
            Just dir | Just dir /= current -> do
                Concurrent.throwTo repl_thread (SaveFileChanged dir)
                go (Just dir) hdl
            _ -> go current hdl

data SaveFileChanged = SaveFileChanged FilePath deriving (Show)
instance Exception.Exception SaveFileChanged


repl :: FilePath -> Haskeline.Settings IO -> IO ()
repl socket settings = Exception.mask (loop settings)
    where
    loop settings restore = do
        status <- restore (Haskeline.runInputT settings
            (Haskeline.withInterrupt
                (read_eval_print (Haskeline.historyFile settings))))
            `Exception.catches`
             [ Exception.Handler $ \(SaveFileChanged save_fname) -> do
                putStrLn $ "save file changed: " ++ save_fname
                return $ Continue (Just save_fname)
             , Exception.Handler $ \Haskeline.Interrupt -> do
                putStrLn "interrupted"
                return $ Continue Nothing
             ]
        case status of
            Continue Nothing -> loop settings restore
            Continue (Just save_fname) -> loop
                (settings { Haskeline.historyFile = Just save_fname })
                restore
            Quit -> return ()

    read_eval_print maybe_save = get_input maybe_save >>= \case
        Nothing -> return Quit
        Just input
            | null input -> return $ Continue Nothing
            | otherwise -> do
                response <- liftIO $ Exception.handle catch_all $
                    ReplUtil.format_response <$>
                        SendCmd.send socket (Text.pack input)
                unless (Text.null response) $
                    liftIO $ Text.IO.putStrLn response
                return $ Continue Nothing
    catch_all :: Exception.SomeException -> IO Text.Text
    catch_all exc = return $ "error: " <> Text.pack (show exc)

get_input :: Maybe FilePath -> Input (Maybe String)
get_input maybe_fname =
    fmap Seq.strip <$> Haskeline.getInputLine (prompt maybe_fname)

data Status = Continue (Maybe FilePath) | Quit deriving (Show)

save_dir_of :: Text -> Maybe FilePath
save_dir_of msg =
    flip FilePath.replaceExtension "repl" . untxt <$> Map.lookup "save" status
    where status = Process.match_pattern Process.global_status_pattern msg

-- | Colorize the prompt to make it stand out.
prompt :: Maybe FilePath -> String
prompt maybe_save = save ++ cyan_bg ++ "入" ++ plain_bg ++ " "
    where
    save = maybe "" (fst . Seq.drop_suffix ".repl" . FilePath.takeFileName)
        maybe_save

-- The trailing \STX tells haskeline this is a control sequence, from
-- http://trac.haskell.org/haskeline/wiki/ControlSequencesInPrompt
cyan_bg :: String
cyan_bg = "\ESC[46m\STX"

plain_bg :: String
plain_bg = "\ESC[39;49m\STX"
