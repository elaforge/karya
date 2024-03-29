-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Simple repl to talk to seq.
module App.Repl (main) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Console.Haskeline as Haskeline
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.Posix.Temp as Posix.Temp
import qualified System.Process as Process

import qualified Util.Control as Control
import qualified Util.Exceptions as Exceptions
import qualified Util.Log as Log
import qualified Util.Network as Network
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists
import qualified Util.Thread as Thread

import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import qualified LogView.Tail as Tail

import           Global


repl_doc :: Text
repl_doc =
    "Type a command to send it to the sequencer.  Everything in\n\
    \Cmd.Repl.Environ, Cmd.Repl.Global, and Local.Repl is in scope.\n\
    \^D to quit.\n\
    \\n\
    \The prompt will have the name of the currently loaded score.  It will be\n\
    \red if there is no connection to Karya.\n\
    \\n\
    \Line editing can be configured with ~/.haskeline, see haskeline docs\n\
    \for details.  Tab completion should work for function names, and\n\
    \filename completion within quotes.  History is saved per-score, so\n\
    \you should have access to previous commands run on this score.\n\
    \\n\
    \Some commands can open an editor on a text file, which is vim only for\n\
    \now.  Saving the file will send its contents back to Karya.  If you have\n\
    \an editor open, Karya will not want to quit, but if you force it to\n\
    \anyway, the editor will be killed and unsaved changes preserved in\n\
    \log/.\n\
    \\n\
    \Colon commands:\n"
    <> colon_doc

type Input a = Haskeline.InputT IO a

initial_settings :: Haskeline.Settings IO
initial_settings = Haskeline.defaultSettings
    { Haskeline.historyFile = Just history_suffix
    , Haskeline.autoAddHistory = True
    }

history_suffix :: FilePath
history_suffix = ".repl"

complete :: Network.Addr -> (String, String)
    -> IO (String, [Haskeline.Completion])
complete addr =
    Haskeline.completeQuotedWord (Just '\\') "\"" Haskeline.listFiles
        (complete_identefier addr)
    -- Like ghci, complete filenames within quotes.  It's useful for save and
    -- load.

complete_identefier :: Network.Addr -> Haskeline.CompletionFunc IO
complete_identefier addr =
    Haskeline.completeWord Nothing word_break_chars complete
    where
    complete prefix = do
        words <- ReplProtocol.query_completion addr (txt prefix)
        return $ map (Haskeline.simpleCompletion . untxt) words
    word_break_chars = " \t\n(),;[]`{}!#$%&*+/<=>?@\\^|-~"

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- System.Environment.getArgs
    addr <- case args of
        [] -> return $ Network.Unix Config.repl_socket_name
        [fn] -> return $ Network.Unix fn
        _ -> errorIO "usage: repl [ unix-socket ]"
    -- I don't want to see "thread started" logs.
    Log.configure $ \state -> state { Log.state_priority = Log.Notice }
    repl_thread <- Concurrent.myThreadId
    Thread.start $ watch_for_quit repl_thread
    liftIO $ putStrLn "^D to quit"
    repl addr $ Haskeline.setComplete (complete addr) initial_settings

-- | Watch the seq.log for state changes, and interrupt the repl to pick up the
-- new state.  Otherwise, it will only notice the change on the next time the
-- prompt comes up, which means the current cmd probably goes into the wrong
-- history.
--
-- This will abort any half-written entry, or any editor in progress, which
-- might be a problem.
watch_for_quit :: Concurrent.ThreadId -> IO ()
watch_for_quit repl_thread = do
    fname <- Tail.log_filename
    loop =<< Tail.open fname (Just 0)
    where
    loop hdl = do
        (msg, hdl) <- Tail.tail hdl
        when (Log.msg_text msg `elem` [Tail.starting_msg, Tail.quitting_msg]) $
            Concurrent.throwTo repl_thread Haskeline.Interrupt
        loop hdl

repl :: Network.Addr -> Haskeline.Settings IO -> IO ()
repl addr settings = Exception.mask $ \restore ->
    Control.loop1 settings $ \loop old_settings -> do
        maybe_save_fname <- ReplProtocol.query_save_file addr
        let (connection_error, settings) = case maybe_save_fname of
                Nothing -> (True, old_settings)
                Just fname -> (,) False $ old_settings
                    { Haskeline.historyFile =
                        Just $ fromMaybe "" fname <> history_suffix
                    }
        -- I would rather replace the prompt in-place, but an interrupt will
        -- always output a newline.  I can't tell where this happens in the
        -- haskeline source, but it happens before the catch gets called, so I
        -- think there's no way to disable it.
        status <- Haskeline.handleInterrupt catch $ restore $
            Haskeline.runInputT settings $ Haskeline.withInterrupt $
            read_eval_print addr connection_error
                (Haskeline.historyFile settings)
        continue <- case status of
            Continue -> return True
            Command cmd -> do
                status <- liftIO $ send_command addr cmd
                case status of
                    Continue -> return True
                    Command cmd -> do
                        -- Or maybe I should just keep having this conversation?
                        putStrLn $ "two Commands in a row: " <> show cmd
                        return True
                    Quit -> return False
            Quit -> return False
        when continue $ loop settings
    where
    read_eval_print addr connection_error history =
        maybe (return Quit) (liftIO . eval addr history . Text.strip)
            =<< get_input connection_error history
    catch = return Continue

eval :: Network.Addr -> Maybe FilePath -> Text -> IO Status
eval addr maybe_history expr = case Text.stripPrefix ":" (Text.toLower expr) of
    Just "h" -> case maybe_history of
        Nothing -> putStrLn "no history to edit" >> return Continue
        Just history -> maybe (return Continue) (send_command addr)
            =<< edit_line history
    Just "r" -> send_command addr ":reload" -- reload handled in ReplGhc
    Just "?" -> Text.IO.putStr repl_doc >> return Continue
    Just _ -> do
        putStrLn $ "unknown colon command " <> show expr <> ", :? for help"
        return Continue
    _ -> send_command addr expr

colon_doc :: Text
colon_doc =
    ":? - Help.\n\
    \:h - Open an editor on the history.  You can find a line, edit it, and\n\
    \     use ZZ to write it back.\n\
    \:r - Reload modified modules.  Only modify \"surface\" modules, e.g.\n\
    \    ones in Cmd/Repl/, since the GHC API tends to crash if you make it\n\
    \    reload too much.  Maybe it crashes if it has to reload something\n\
    \    with a C dependency.\n"

send_command :: Network.Addr -> Text -> IO Status
send_command addr expr
    | Text.null expr = return Continue
    | otherwise = do
        result <- ReplProtocol.query_cmd addr (Text.strip expr)
        result <- print_logs result
        handle_result addr result

handle_result :: Network.Addr -> ReplProtocol.Result -> IO Status
handle_result addr = \case
    ReplProtocol.Raw text -> do
        unless (Text.null (Text.strip text)) $
            Text.IO.putStrLn (Text.stripEnd text)
        return Continue
    ReplProtocol.Format text -> do
        unless (Text.null (Text.strip text)) $
            putStr $ PPrint.format_str $ untxt text
        return Continue
    ReplProtocol.Edit editors -> do
        ReplProtocol.notify addr ReplProtocol.NEditorOpened
        edit_multiple editors
        ReplProtocol.notify addr ReplProtocol.NEditorClosed
        return Continue

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

get_input :: Bool -> Maybe FilePath -> Input (Maybe Text)
get_input connection_error history =
    fmap (Text.strip . txt) <$>
        Haskeline.getInputLine (prompt connection_error history)

data Status = Continue
    -- | Skip the next prompt and send this as a QCommand.
    | Command !Text
    -- | Blow this popsicle stand.
    | Quit deriving (Show)

-- | Colorize the prompt to make it stand out.
prompt :: Bool -> Maybe FilePath -> String
prompt connection_error maybe_save =
    mconcat [save,  color_bg,  stx,  "入",  plain_bg,  stx,  " "]
    where
    color_bg = if connection_error then red_bg else cyan_bg
    save = maybe "" (fst . Lists.dropSuffix ".repl" . FilePath.takeFileName)
        maybe_save
    -- The trailing \STX tells haskeline this is a control sequence, from
    -- http://trac.haskell.org/haskeline/wiki/ControlSequencesInPrompt
    stx = "\STX"

cyan_bg :: String
cyan_bg = "\ESC[46m"

red_bg :: String
red_bg = "\ESC[41m"

plain_bg :: String
plain_bg = "\ESC[39;49m"


-- * editor

-- | Edit multiple files, each with their own vim config.  This blocks until
-- the editor exits.
--
-- The technique is to make a file of vim commands that edits each file and
-- runs commands in turn.
--
-- TODO currently only vim is supported, but I assume other editors could also
-- be press-ganged into similar service.
edit_multiple :: NonEmpty ReplProtocol.Editor -> IO ()
edit_multiple edits_ = with_files (map ReplProtocol._file edits) $ \fnames ->
    with_temp "repl-cmds-" ".vim" (make_cmds fnames) $ \cmd_fname -> do
        ok <- wait_for_command "vim"
            [ "-s", cmd_fname
            , "-c", "source vim-functions.vim"
            -- Put the swp file in the log/ directory.  Otherwise it's in a tmp
            -- dir and dies when it does.  This is just paranoia, if vim happens
            -- to get killed with unsaved data (say due to 'watch_for_quit'),
            -- then it can be recovered.
            , "-c", ":set directory=log"
            ]
        when ok $ forM_ (zip fnames (map ReplProtocol._on_save edits)) $ \case
            (fname, Just on_save) -> send_file fname on_save
            _ -> return ()
        return ()
    where
    edits = NonEmpty.toList edits_
    make_cmds fnames = Text.unlines $
        concatMap edit_cmds (zip fnames edits) ++ [":buffer 1"]
    edit_cmds (fname, ReplProtocol.Editor _ linenum on_save on_send) = concat
        [ [ ":edit " <> txt fname]
        , [":" <> showt linenum]
        , [ ":nmap <buffer> gz :call Send('" <> c <> "')<cr>"
          | Just c <- [on_save]
          ]
        , [ ":nmap <buffer> gs :call Send('" <> c <> "')<cr>"
          | Just c <- [on_send]
          ]
        ]

with_files :: [ReplProtocol.File] -> ([FilePath] -> IO a) -> IO a
with_files files action = go [] files
    where
    go accum [] = action (reverse accum)
    go accum (ReplProtocol.FileName fname : files) = go (fname:accum) files
    go accum (ReplProtocol.Text ftype content : files) = do
        let ext = ReplProtocol.file_type_extension ftype
        with_temp "repl-" ext content $ \fname -> go (fname:accum) files

send_binary :: FilePath
send_binary = "build/opt/send"

send_file :: FilePath -> Text -> IO ()
send_file fname cmd = do
    -- The 'send' cmd substitutes stdin for %s.
    content <- if "%s" `Text.isInfixOf` cmd
        -- vim will add a final newline.
        then Text.stripEnd <$> Text.IO.readFile fname
        else return ""
    -- Sometimes I forget to build it and it's annoying to crash the REPL.
    unlessM (Directory.doesFileExist send_binary) $
        Process.callProcess "bin/mk" [send_binary]
    stdout <- Process.readProcess send_binary [untxt cmd]
        (untxt content)
    unless (null stdout) $
        putStrLn $ "send: " <> stdout

-- | Run the action with a temp file, and delete it afterwards.
with_temp :: FilePath -> String -> Text -> (FilePath -> IO a) -> IO a
with_temp prefix suffix contents action = do
    -- .ky prefix so the vim autocmds will fire.
    (path, hdl) <- Posix.Temp.mkstemps prefix suffix
    Text.IO.hPutStr hdl contents
    IO.hClose hdl
    action path
        `Exception.finally` Exceptions.ignoreEnoent (Directory.removeFile path)

-- | Open the given file, and return the selected line.
edit_line :: FilePath -> IO (Maybe Text)
edit_line fname = with_temp "repl-edit-history-" "" "" $ \tmp -> do
    let cmdline =
            [ "-c", "nmap ZZ :set write \\| .w! " <> tmp <> " \\| q!<cr>"
            , "-c", "set nowrite"
            , fname
            ]
    ifM (wait_for_command "vim" cmdline)
        (Just . Text.strip <$> Text.IO.readFile tmp)
        (return Nothing)

-- | This will abort the editor on a force-quit from karya.  If I leave it
-- open, it's too easy to forget, and then load another score, and it wants
-- to write to the wrong score.
--
-- TODO: I would much rather have this trigger the editor to save and quit but
-- for that I'd need a two way shutdown protocol.
wait_for_command :: FilePath -> [String] -> IO Bool
wait_for_command cmd args = do
    tty_state <- Text.strip <$> capture "stty" ["-g"]
    (_, _, _, pid) <- Process.createProcess $
        (Process.proc cmd args) { Process.delegate_ctlc = True }
    Exception.try (Process.waitForProcess pid) >>= \case
        Left Haskeline.Interrupt -> do
            Process.terminateProcess pid
            -- Without this the terminal wedges, probably due to both vim
            -- and haskeline wanting to mess with terminal state.
            Process.callProcess "stty" [untxt tty_state]
            _ <- Process.waitForProcess pid
            putStrLn "Interrupted by karya quitting, editor killed and tty\
                \ state restored."
            putStrLn "Any unsaved edits saved as log/*.swp, vim -r log/*.swp\
                \ to recover."
            return False
        Right Exit.ExitSuccess -> return True
        Right (Exit.ExitFailure code) -> do
            -- Maybe the binary wasn't found, but vim seems to return 1
            -- unpredictably.
            Log.warn $ "non-zero exit code from "
                <> showt (cmd : args) <> ": " <> showt code
            return False

-- This can't use readProcess, which always pipes stdin, which stty doesn't
-- likek.
capture :: String -> [String] -> IO Text
capture cmd args =
    Process.withCreateProcess proc $ \Nothing (Just outh) Nothing pid -> do
        stdout <- Text.IO.hGetContents outh
        -- TODO check for error
        Process.waitForProcess pid
        return stdout
    where
    proc = (Process.proc cmd args) { Process.std_out = Process.CreatePipe }
