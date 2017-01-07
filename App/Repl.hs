-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase #-}
{- | Simple repl to talk to seq.

    Type a command to send it to the sequencer.  Everything in
    "Cmd.Repl.Environ" and "Cmd.Repl.Global" is in scope.

    The prompt will have the name of the currently loaded score, and history
    will be written to (and read from) a name.repl file.  Unfortunately you
    have to hit enter to update it if it changed.  TODO bring back async
    notification like before?

    Tab completion should work for function names, and filename completion
    within quotes.

    @:r@ or @:R@ will reload modified modules, but only modify "surface"
    modules, since the GHC API tends to crash if you make it reload too much.
    Maybe crashes if it has to reload something with a C dependency.

    @:h@ or @:H@ will open an editor on the history.  You can find a line, edit
    it, and use ZZ to write it back.
-}
module App.Repl where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Network
import qualified System.Console.Haskeline as Haskeline
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.Posix.Temp as Posix.Temp
import qualified System.Process as Process

import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import Global


type Input a = Haskeline.InputT IO a

initial_settings :: Haskeline.Settings IO
initial_settings = Haskeline.defaultSettings
    { Haskeline.historyFile = Just history_suffix
    , Haskeline.autoAddHistory = True
    }

history_suffix :: FilePath
history_suffix = ".repl"

complete :: Network.PortID -> (String, String)
    -> IO (String, [Haskeline.Completion])
complete socket =
    Haskeline.completeQuotedWord (Just '\\') "\"" Haskeline.listFiles
        (complete_identefier socket)
    -- Like ghci, complete filenames within quotes.
    -- TODO or just disable completion?

complete_identefier :: Network.PortID -> Haskeline.CompletionFunc IO
complete_identefier socket =
    Haskeline.completeWord Nothing word_break_chars complete
    where
    complete prefix = do
        words <- ReplProtocol.query_completion socket (txt prefix)
        return $ map (Haskeline.simpleCompletion . untxt) words
    word_break_chars = " \t\n(),;[]`{}!#$%&*+/<=>?@\\^|-~"

type CurrentHistory = MVar.MVar (Maybe FilePath)

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- System.Environment.getArgs
    socket <- case args of
        [] -> return Config.repl_socket
        [fn] -> return $ Network.UnixSocket fn
        _ -> errorIO $ "usage: repl [ unix-socket ]"
    -- I don't want to see "thread started" logs.
    Log.configure $ \state -> state { Log.state_log_level = Log.Notice }
    liftIO $ putStrLn "^D to quit"
    repl socket $ Haskeline.setComplete (complete socket) initial_settings

data SaveFileChanged = SaveFileChanged FilePath deriving (Show)
instance Exception.Exception SaveFileChanged

repl :: Network.PortID -> Haskeline.Settings IO -> IO ()
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
                    { Haskeline.historyFile =
                        Just $ fromMaybe "" fname <> history_suffix
                    }
        status <- Exception.handle catch $ restore $
            Haskeline.runInputT settings $ Haskeline.withInterrupt
                (read_eval_print socket (Haskeline.historyFile settings))
        case status of
            Continue -> loop settings restore
            Command cmd -> do
                status <- liftIO $ send_command socket cmd
                case status of
                    Continue -> loop settings restore
                    Command cmd -> do
                        -- Or maybe I should just keep having this conversation?
                        putStrLn $ "two Commands in a row: " <> show cmd
                        loop settings restore
                    Quit -> return ()
            Quit -> return ()
    read_eval_print socket history =
        maybe (return Quit) (liftIO . eval socket history) =<< get_input history

eval :: Network.PortID -> Maybe FilePath -> Text -> IO Status
eval socket maybe_history expr
    | Text.strip expr `elem` [":h", ":H"] = case maybe_history of
        Nothing -> putStrLn "no history to edit" >> return Continue
        Just history ->
            maybe (return Continue) (send_command socket) =<< edit_line history
    | otherwise = send_command socket expr

send_command :: Network.PortID -> Text -> IO Status
send_command socket expr
    | Text.null expr = return Continue
    | otherwise = do
        result <- ReplProtocol.query_cmd socket (Text.strip expr)
        result <- print_logs result
        handle_result result

handle_result :: ReplProtocol.Result -> IO Status
handle_result (ReplProtocol.Raw text) = do
    unless (Text.null (Text.strip text)) $
        Text.IO.putStrLn (Text.stripEnd text)
    return Continue
handle_result (ReplProtocol.Format text) = do
    unless (Text.null (Text.strip text)) $
        putStr $ PPrint.format_str $ untxt text
    return Continue
handle_result (ReplProtocol.Edit text return_prefix) =
    maybe Continue (\edited -> Command $ return_prefix <> " " <> showt edited)
        <$> edit (Just return_prefix) text

print_logs :: ReplProtocol.CmdResult -> IO ReplProtocol.Result
print_logs (ReplProtocol.CmdResult val logs_) = do
    -- Filter Debug logs, otherwise I get spammed with cache msgs.
    let logs = filter ((>Log.Debug) . Log.msg_priority) $
            ReplProtocol.abbreviate_package_loads logs_
    unless (null logs) $ do
        putStrLn "Logs:"
        mapM_ Pretty.pprint logs
        putChar '\n'
    return val

get_input :: Maybe FilePath -> Input (Maybe Text)
get_input history =
    fmap (Text.strip . txt) <$> Haskeline.getInputLine (prompt history)

data Status = Continue
    -- | Skip the next prompt and send this as a QCommand.
    | Command !Text
    -- | Blow this popsicle stand.
    | Quit deriving (Show)

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


-- * editor

-- | Open an editor on the given text, and return what it saves.
-- TODO maybe use $EDITOR instead of hardcoding vi.
edit :: Maybe Text -- ^ if given, send the file with this cmd prefix on save
    -> Text -> IO (Maybe Text)
edit return_prefix text =
    edit_temp_file "repl-" text "vi" (\tmp -> save_cmd ++ [tmp])
    where
    save_cmd = case return_prefix of
        Nothing -> []
        Just prefix ->
            [ "-c", "nmap gz :wa<cr>:!build/opt/send --cmd '" <> untxt prefix
                <> "' <%<cr>"
            -- I don't know that it's ky syntax, but so far it is.
            , "-c", "source ky-syntax.vim"
            ]

-- | Open the given file, and return the selected line.
edit_line :: FilePath -> IO (Maybe Text)
edit_line fname = edit_temp_file "repl-edit-history-" "" "vi" cmdline
    where
    cmdline tmp =
        [ "-c", "nmap ZZ :set write \\| .w! " <> tmp <> " \\| q!<cr>"
        , "-c", "set nowrite"
        , fname
        ]

edit_temp_file :: FilePath -> Text -> FilePath -> (FilePath -> [String])
    -> IO (Maybe Text)
edit_temp_file prefix contents cmd args = do
    (path, hdl) <- Posix.Temp.mkstemp prefix
    Text.IO.hPutStr hdl contents
    Text.IO.hPutStr hdl "\n" -- otherwise vim doesn't like no final newline
    IO.hClose hdl
    action path
        `Exception.finally` File.ignoreEnoent (Directory.removeFile path)
    where
    action path = do
        pid <- Process.spawnProcess cmd (args path)
        code <- Process.waitForProcess pid
        case code of
            Exit.ExitSuccess -> do
                edited <- Text.IO.readFile path
                -- vim will add a final newline.
                return $ Just (Text.stripEnd edited)
            Exit.ExitFailure code -> do
                Log.warn $ "non-zero exit code from editor "
                    <> showt (cmd : args path) <> ": " <> showt code
                return Nothing
