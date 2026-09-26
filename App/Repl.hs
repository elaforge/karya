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


replDoc :: Text
replDoc =
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
    <> colonDoc

type Input a = Haskeline.InputT IO a

initialSettings :: Haskeline.Settings IO
initialSettings = Haskeline.defaultSettings
    { Haskeline.historyFile = Just historySuffix
    , Haskeline.autoAddHistory = True
    }

historySuffix :: FilePath
historySuffix = ".repl"

complete :: Network.Addr -> (String, String)
    -> IO (String, [Haskeline.Completion])
complete addr =
    Haskeline.completeQuotedWord (Just '\\') "\"" Haskeline.listFiles
        (completeIdentefier addr)
    -- Like ghci, complete filenames within quotes.  It's useful for save and
    -- load.

completeIdentefier :: Network.Addr -> Haskeline.CompletionFunc IO
completeIdentefier addr =
    Haskeline.completeWord Nothing wordBreakChars complete
    where
    complete prefix = do
        words <- ReplProtocol.queryCompletion addr (txt prefix)
        pure $ map (Haskeline.simpleCompletion . untxt) words
    wordBreakChars = " \t\n(),;[]`{}!#$%&*+/<=>?@\\^|-~"

main :: IO ()
main = ReplProtocol.initialize $ do
    args <- System.Environment.getArgs
    addr <- case args of
        [] -> pure $ Network.Unix Config.replSocketName
        [fn] -> pure $ Network.Unix fn
        _ -> errorIO "usage: repl [ unix-socket ]"
    -- I don't want to see "thread started" logs.
    Log.configure $ \state -> state { Log.statePriority = Log.Notice }
    replThread <- Concurrent.myThreadId
    Thread.start $ watchForQuit replThread
    liftIO $ putStrLn "^D to quit"
    repl addr $ Haskeline.setComplete (complete addr) initialSettings

-- | Watch the seq.log for state changes, and interrupt the repl to pick up the
-- new state.  Otherwise, it will only notice the change on the next time the
-- prompt comes up, which means the current cmd probably goes into the wrong
-- history.
--
-- This will abort any half-written entry, or any editor in progress, which
-- might be a problem.
watchForQuit :: Concurrent.ThreadId -> IO ()
watchForQuit replThread = do
    fname <- Tail.logFilename
    loop =<< Tail.open fname (Just 0)
    where
    loop hdl = do
        (msg, hdl) <- Tail.tail hdl
        when (Log.msgText msg `elem` [Tail.startingMsg, Tail.quittingMsg]) $
            Concurrent.throwTo replThread Haskeline.Interrupt
        loop hdl

repl :: Network.Addr -> Haskeline.Settings IO -> IO ()
repl addr settings = Exception.mask $ \restore ->
    Control.loop1 settings $ \loop oldSettings -> do
        maybeSaveFname <- ReplProtocol.querySaveFile addr
        let (connectionError, settings) = case maybeSaveFname of
                Nothing -> (True, oldSettings)
                Just fname -> (,) False $ oldSettings
                    { Haskeline.historyFile =
                        Just $ fromMaybe "" fname <> historySuffix
                    }
        -- I would rather replace the prompt in-place, but an interrupt will
        -- always output a newline.  I can't tell where this happens in the
        -- haskeline source, but it happens before the catch gets called, so I
        -- think there's no way to disable it.
        status <- Haskeline.handleInterrupt catch $ restore $
            Haskeline.runInputT settings $ Haskeline.withInterrupt $
            readEvalPrint addr connectionError
                (Haskeline.historyFile settings)
        continue <- case status of
            Continue -> pure True
            Command cmd -> do
                status <- liftIO $ sendCommand addr cmd
                case status of
                    Continue -> pure True
                    Command cmd -> do
                        -- Or maybe I should just keep having this conversation?
                        putStrLn $ "two Commands in a row: " <> show cmd
                        pure True
                    Quit -> pure False
            Quit -> pure False
        when continue $ loop settings
    where
    readEvalPrint addr connectionError history =
        maybe (pure Quit) (liftIO . eval addr history . Text.strip)
            =<< getInput connectionError history
    catch = pure Continue

eval :: Network.Addr -> Maybe FilePath -> Text -> IO Status
eval addr maybeHistory expr = case Text.stripPrefix ":" (Text.toLower expr) of
    Just "h" -> case maybeHistory of
        Nothing -> putStrLn "no history to edit" >> pure Continue
        Just history -> maybe (pure Continue) (sendCommand addr)
            =<< editLine history
    Just "r" -> sendCommand addr ":reload" -- reload handled in ReplGhc
    Just "?" -> Text.IO.putStr replDoc >> pure Continue
    Just _ -> do
        putStrLn $ "unknown colon command " <> show expr <> ", :? for help"
        pure Continue
    _ -> sendCommand addr expr

colonDoc :: Text
colonDoc =
    ":? - Help.\n\
    \:h - Open an editor on the history.  You can find a line, edit it, and\n\
    \     use ZZ to write it back.\n\
    \:r - Reload modified modules.  Only modify \"surface\" modules, e.g.\n\
    \    ones in Cmd/Repl/, since the GHC API tends to crash if you make it\n\
    \    reload too much.  Maybe it crashes if it has to reload something\n\
    \    with a C dependency.\n"

sendCommand :: Network.Addr -> Text -> IO Status
sendCommand addr expr
    | Text.null expr = pure Continue
    | otherwise = do
        result <- ReplProtocol.queryCmd addr (Text.strip expr)
        result <- printLogs result
        handleResult addr result

handleResult :: Network.Addr -> ReplProtocol.Result -> IO Status
handleResult addr = \case
    ReplProtocol.Raw text -> do
        unless (Text.null (Text.strip text)) $
            Text.IO.putStrLn (Text.stripEnd text)
        pure Continue
    ReplProtocol.Format text -> do
        unless (Text.null (Text.strip text)) $
            putStr $ PPrint.formatStr $ untxt text
        pure Continue
    ReplProtocol.Edit editors -> do
        ReplProtocol.notify addr ReplProtocol.NEditorOpened
        editMultiple editors
        ReplProtocol.notify addr ReplProtocol.NEditorClosed
        pure Continue

printLogs :: ReplProtocol.CmdResult -> IO ReplProtocol.Result
printLogs (ReplProtocol.CmdResult val logs_) = do
    -- Filter Debug logs, otherwise I get spammed with cache msgs.
    let logs = filter ((>Log.Debug) . Log.msgPriority) $
            ReplProtocol.abbreviatePackageLoads logs_
    unless (null logs) $ do
        putStrLn "Logs:"
        mapM_ Pretty.pprint logs
        putChar '\n'
    pure val

getInput :: Bool -> Maybe FilePath -> Input (Maybe Text)
getInput connectionError history =
    fmap (Text.strip . txt) <$>
        Haskeline.getInputLine (prompt connectionError history)

data Status = Continue
    -- | Skip the next prompt and send this as a QCommand.
    | Command !Text
    -- | Blow this popsicle stand.
    | Quit deriving (Show)

-- | Colorize the prompt to make it stand out.
prompt :: Bool -> Maybe FilePath -> String
prompt connectionError maybeSave =
    mconcat [save,  colorBg,  stx,  "入",  plainBg,  stx,  " "]
    where
    colorBg = if connectionError then redBg else cyanBg
    save = maybe "" (fst . Lists.dropSuffix ".repl" . FilePath.takeFileName)
        maybeSave
    -- The trailing \STX tells haskeline this is a control sequence, from
    -- http://trac.haskell.org/haskeline/wiki/ControlSequencesInPrompt
    stx = "\STX"

cyanBg :: String
cyanBg = "\ESC[46m"

redBg :: String
redBg = "\ESC[41m"

plainBg :: String
plainBg = "\ESC[39;49m"


-- * editor

-- | Edit multiple files, each with their own vim config.  This blocks until
-- the editor exits.
--
-- The technique is to make a file of vim commands that edits each file and
-- runs commands in turn.
--
-- TODO currently only vim is supported, but I assume other editors could also
-- be press-ganged into similar service.
editMultiple :: NonEmpty ReplProtocol.Editor -> IO ()
editMultiple edits_ = withFiles (map ReplProtocol._file edits) $ \fnames ->
    withTemp "repl-cmds-" ".vim" (makeCmds fnames) $ \cmdFname -> do
        ok <- waitForCommand "vim"
            [ "-s", cmdFname
            , "-c", "source vim-functions.vim"
            -- Put the swp file in the log/ directory.  Otherwise it's in a tmp
            -- dir and dies when it does.  This is just paranoia, if vim happens
            -- to get killed with unsaved data (say due to 'watch_for_quit'),
            -- then it can be recovered.
            , "-c", ":set directory=log"
            ]
        when ok $ forM_ (zip fnames (map ReplProtocol._onSave edits)) $ \case
            (fname, Just onSave) -> sendFile fname onSave
            _ -> pure ()
        pure ()
    where
    edits = NonEmpty.toList edits_
    makeCmds fnames = Text.unlines $
        concatMap editCmds (zip fnames edits) ++ [":buffer 1"]
    editCmds (fname, ReplProtocol.Editor _ linenum onSave onSend) = concat
        [ [ ":edit " <> txt fname]
        , [":" <> showt linenum]
        , [ ":nmap <buffer> gz :call Send('" <> c <> "')<cr>"
          | Just c <- [onSave]
          ]
        , [ ":nmap <buffer> gs :call Send('" <> c <> "')<cr>"
          | Just c <- [onSend]
          ]
        ]

withFiles :: [ReplProtocol.File] -> ([FilePath] -> IO a) -> IO a
withFiles files action = go [] files
    where
    go accum [] = action (reverse accum)
    go accum (ReplProtocol.FileName fname : files) = go (fname:accum) files
    go accum (ReplProtocol.Text ftype content : files) = do
        let ext = ReplProtocol.fileTypeExtension ftype
        withTemp "repl-" ext content $ \fname -> go (fname:accum) files

sendBinary :: FilePath
sendBinary = "build/opt/send"

sendFile :: FilePath -> Text -> IO ()
sendFile fname cmd = do
    -- The 'send' cmd substitutes stdin for %s.
    content <- if "%s" `Text.isInfixOf` cmd
        -- vim will add a final newline.
        then Text.stripEnd <$> Text.IO.readFile fname
        else pure ""
    -- Sometimes I forget to build it and it's annoying to crash the REPL.
    unlessM (Directory.doesFileExist sendBinary) $
        Process.callProcess "bin/mk" [sendBinary]
    stdout <- Process.readProcess sendBinary [untxt cmd]
        (untxt content)
    unless (null stdout) $
        putStrLn $ "send: " <> stdout

-- | Run the action with a temp file, and delete it afterwards.
withTemp :: FilePath -> String -> Text -> (FilePath -> IO a) -> IO a
withTemp prefix suffix contents action = do
    -- .ky prefix so the vim autocmds will fire.
    (path, hdl) <- Posix.Temp.mkstemps prefix suffix
    Text.IO.hPutStr hdl contents
    IO.hClose hdl
    action path
        `Exception.finally` Exceptions.ignoreEnoent (Directory.removeFile path)

-- | Open the given file, and return the selected line.
editLine :: FilePath -> IO (Maybe Text)
editLine fname = withTemp "repl-edit-history-" "" "" $ \tmp -> do
    let cmdline =
            [ "-c", "nmap ZZ :set write \\| .w! " <> tmp <> " \\| q!<cr>"
            , "-c", "set nowrite"
            , fname
            ]
    ifM (waitForCommand "vim" cmdline)
        (Just . Text.strip <$> Text.IO.readFile tmp)
        (pure Nothing)

-- | This will abort the editor on a force-quit from karya.  If I leave it
-- open, it's too easy to forget, and then load another score, and it wants
-- to write to the wrong score.
--
-- TODO: I would much rather have this trigger the editor to save and quit but
-- for that I'd need a two way shutdown protocol.
waitForCommand :: FilePath -> [String] -> IO Bool
waitForCommand cmd args = do
    ttyState <- Text.strip <$> capture "stty" ["-g"]
    (_, _, _, pid) <- Process.createProcess $
        (Process.proc cmd args) { Process.delegateCtlc = True }
    Exception.try (Process.waitForProcess pid) >>= \case
        Left Haskeline.Interrupt -> do
            Process.terminateProcess pid
            -- Without this the terminal wedges, probably due to both vim
            -- and haskeline wanting to mess with terminal state.
            Process.callProcess "stty" [untxt ttyState]
            _ <- Process.waitForProcess pid
            putStrLn "Interrupted by karya quitting, editor killed and tty\
                \ state restored."
            putStrLn "Any unsaved edits saved as log/*.swp, vim -r log/*.swp\
                \ to recover."
            pure False
        Right Exit.ExitSuccess -> pure True
        Right (Exit.ExitFailure code) -> do
            -- Maybe the binary wasn't found, but vim seems to return 1
            -- unpredictably.
            Log.warn $ "non-zero exit code from "
                <> showt (cmd : args) <> ": " <> showt code
            pure False

-- This can't use readProcess, which always pipes stdin, which stty doesn't
-- like.
capture :: String -> [String] -> IO Text
capture cmd args =
    Process.withCreateProcess proc $ \Nothing (Just outh) Nothing pid -> do
        stdout <- Text.IO.hGetContents outh
        -- TODO check for error
        Process.waitForProcess pid
        pure stdout
    where
    proc = (Process.proc cmd args) { Process.std_out = Process.CreatePipe }
