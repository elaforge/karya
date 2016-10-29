-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase #-}
-- | Simple repl to talk to seq.
module App.Repl where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Console.Haskeline as Haskeline
import qualified System.Environment
import qualified System.FilePath as FilePath

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import Global


type Input a = Haskeline.InputT IO a

initial_settings :: Haskeline.Settings IO
initial_settings = Haskeline.setComplete complete $ Haskeline.defaultSettings
    { Haskeline.historyFile = Nothing
    , Haskeline.autoAddHistory = True
    }

complete :: (String, String) -> IO (String, [Haskeline.Completion])
complete =
    Haskeline.completeQuotedWord (Just '\\') "\"" Haskeline.listFiles
        complete_identefier
    -- Like ghci, complete filenames within quotes.
    -- TODO or just disable completion?

complete_identefier :: Haskeline.CompletionFunc IO
complete_identefier = Haskeline.completeWord Nothing word_break_chars complete
    where
    complete prefix = do
        words <- ReplProtocol.query_completion Config.repl_port (txt prefix)
        return $ map (Haskeline.simpleCompletion . untxt) words

word_break_chars :: String
word_break_chars =
    " \t\n\
    \(),;[]`{}\
    \!#$%&*+/<=>?@\\^|-~"

type CurrentHistory = MVar.MVar (Maybe FilePath)

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- System.Environment.getArgs
    socket <- case args of
        [] -> return Config.repl_port
        [fn] -> return fn
        _ -> errorIO $ "usage: repl [ unix-socket ]"
    -- I don't want to see "thread started" logs.
    Log.configure $ \state -> state { Log.state_log_level = Log.Notice }
    liftIO $ putStrLn "^D to quit"
    repl socket initial_settings

data SaveFileChanged = SaveFileChanged FilePath deriving (Show)
instance Exception.Exception SaveFileChanged

repl :: FilePath -> Haskeline.Settings IO -> IO ()
repl socket settings = Exception.mask (loop settings)
    where
    loop old_settings restore = do
        let catch Haskeline.Interrupt = do
                putStrLn "interrupted"
                return Continue
        maybe_save_fname <- ReplProtocol.query_save_file socket
        let settings = case maybe_save_fname of
                Nothing -> old_settings
                Just fname -> old_settings
                    { Haskeline.historyFile = (<>".repl") <$> fname }
        status <- Exception.handle catch $ restore $
            Haskeline.runInputT settings $ Haskeline.withInterrupt
                (read_eval_print (Haskeline.historyFile settings))
        case status of
            Continue -> loop settings restore
            Quit -> return ()
    read_eval_print maybe_save = get_input maybe_save >>= \case
        Nothing -> return Quit
        Just input
            | null input -> return Continue
            | otherwise -> do
                result <- liftIO $ ReplProtocol.query_cmd socket $
                    Text.strip $ Text.pack input
                let text = ReplProtocol.format_result result
                unless (Text.null text) $
                    liftIO $ Text.IO.putStrLn text
                return Continue

get_input :: Maybe FilePath -> Input (Maybe String)
get_input maybe_fname =
    fmap Seq.strip <$> Haskeline.getInputLine (prompt maybe_fname)

data Status = Continue | Quit deriving (Show)

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
