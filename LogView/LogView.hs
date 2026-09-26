-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This is a standalone program to monitor the log file.

    It reads lines and streams them to the GUI, which displays them in
    a scrolling box.  Clickable text is surrounded by @{}@s and will be
    highlighted blue in the GUI.  When it's clicked, the GUI sends the tag
    back, much like an HTML href.  For example, stack traces are formatted as
    a REPL cmd that will highlight that location on the score.

    The top line is the status bar, which extracts and remembers bits of
    specially formatted log msgs.  This effectively functions as the app's
    global status bar, since otherwise it has no place for this kind of
    information.  The configuration is 'default_catch_patterns'.

    This also maintains a filter.  The filter is a little language that will
    filter out messages that don't match, documented by
    'Process.compile_filter'.

    In addition, there is a concept of 'Process.CatchPattern's.  These are
    regexes which are matched against msg text.  When one matches, the matched
    groups are kept in a status line.  That way, events reported in the log can
    be collected together.
-}
module LogView.LogView where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Control.Monad.State as State

import qualified Data.ByteString.Char8 as ByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified GHC.IO.Encoding as Encoding
import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.Posix as Posix

import qualified Util.Exceptions as Exceptions
import qualified Util.Fltk as Fltk
import qualified Util.FltkUtil as FltkUtil
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Processes as Processes

import qualified App.Path as Path
import qualified App.ReplProtocol as ReplProtocol
import qualified LogView.LogViewC as LogViewC
import qualified LogView.Process as Process
import qualified LogView.Tail as Tail

import           Global


-- | I use this file to only start one logview at a time.
pidFile :: FilePath
pidFile = "log/logview.pid"

-- | Initial contents of the filter field.
initialFilter :: Text
initialFilter = "**"

initialSize :: (Int, Int)
initialSize = (900, 300)

-- | Built-in list of catch patterns.
--
-- I wound up having the app emit catch patterns explicitly instead of putting
-- the smarts in logview, so now the only CatchPattern is
-- 'Process.global_status_pattern'.  But it still seems conceivable that
-- someday I may want logview to catch things the app didn't explicitly mean to
-- be caught, so I'll leave this functionality in for now.
defaultCatchPatterns :: [Process.CatchPattern]
defaultCatchPatterns = [Process.globalStatusPattern]

-- | Remember this many log msgs.
defaultHistory :: Int
defaultHistory = 1000

-- | UI will remember this many bytes.  This is not the same as
-- 'default_history' because the history will remember filtered out msgs, and
-- the UI doesn't bother to preserve msg boundaries so it uses bytes.
defaultMaxBytes :: Int
defaultMaxBytes = defaultHistory * 100

data Flag = Help | Seek (Maybe Integer) | Print | History Int | File FilePath
    | Geometry FltkUtil.Geometry
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , GetOpt.Option [] ["seek"] (GetOpt.OptArg (Seek . fmap read) "lines") $
        "if given no arg, scan the log file from the beginning, if given an"
        ++ " arg, scan approximately that many lines from the end (assuming"
        ++ " the average line is 200 bytes)"
    , GetOpt.Option [] ["print"] (GetOpt.NoArg Print)
        "print formatted logs to stdout instead of bringing up the GUI"
    , GetOpt.Option [] ["history"]
        (GetOpt.ReqArg (History . read) (show defaultHistory))
        "remember this many lines"
    , GetOpt.Option [] ["file"] (GetOpt.ReqArg File "seq.log")
        "read from this file"
    , FltkUtil.option Geometry
    ]

type LogChan = TChan.TChan Log.Msg

main :: IO ()
main = do
    Encoding.setLocaleEncoding Encoding.utf8
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> pure (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Lists.join ", " errs
    unless (null args) $
        usage ("unparsed args: " ++ show args)
    when (Help `elem` flags) (usage "usage:")
    whenJustM writePid $ \pid -> do
        putStrLn $ "logview already running with pid " ++ show pid
        System.Exit.exitSuccess
    logview flags `Exception.finally` Directory.removeFile pidFile
    where
    usage msg = do
        putStrLn $ "ERROR: " ++ msg
        putStrLn "usage: logview [ flags ]"
        putStr (GetOpt.usageInfo "" options)
        System.Exit.exitFailure

logview :: [Flag] -> IO ()
logview flags = do
    let seek = fromMaybe (Just 0) $ Lists.last [s | Seek s <- flags]
        history = fromMaybe defaultHistory $
            Lists.last [n | History n <- flags]
    filename <- maybe Tail.logFilename pure $
        Lists.last [n | File n <- flags]
    hdl <- Tail.open filename seek
    logChan <- STM.newTChanIO
    guiChan <- if Print `elem` flags then pure Nothing
        else Just <$> Fltk.newChannel
    mainThread <- Concurrent.myThreadId
    Concurrent.forkFinally (tailLoop logChan hdl) $ \result -> do
        putStrLn $ "tail loop died: " ++ either show (const "no exception")
            (result :: Either Exception.SomeException ())
        maybe (Concurrent.killThread mainThread) Fltk.quit guiChan
    case guiChan of
        Nothing -> printLogs logChan
        Just chan -> gui geometry chan logChan filename history
            where geometry = Lists.head [g | Geometry g <- flags]
    where
    tailLoop logChan hdl = do
        (msg, hdl) <- Tail.tail hdl
        STM.atomically $ TChan.writeTChan logChan msg
        tailLoop logChan hdl

-- | If a logview is already running, return its pid, otherwise write the
-- current pid.
writePid :: IO (Maybe Posix.ProcessID)
writePid = do
    -- I have to use ByteString.readFile and writeFile to avoid GHC's obnoxious
    -- file locking.
    pidStr <- Exceptions.ignoreEnoent $ ByteString.readFile pidFile
    existing <- case ByteString.readInt =<< pidStr of
        Just (pid_, _) -> do
            maybeCmd <- Processes.commandName pid
            pure $ if maybe False ("logview" `List.isSuffixOf`) maybeCmd
                then Just pid else Nothing
            where pid = fromIntegral pid_
        _ -> pure Nothing
    when (existing == Nothing) $ do
        pid <- Posix.getProcessID
        ByteString.writeFile pidFile (ByteString.pack (show pid) <> "\n")
    pure existing

gui :: Maybe FltkUtil.Geometry -> Fltk.Channel -> LogChan -> FilePath -> Int
    -> IO ()
gui geometry chan logChan filename history = do
    filename <- Directory.canonicalizePath filename
    let (x, y, w, h) =
            FltkUtil.xywh 20 20 (fst initialSize) (snd initialSize) geometry
    win <- Fltk.runAction $
        LogViewC.create x y w h filename defaultMaxBytes
    Fltk.runAction $ LogViewC.setFilter win initialFilter
    let state = (Process.initialState initialFilter)
            { Process.stateCatchPatterns = defaultCatchPatterns }
    Concurrent.forkIO $ handleMsgs chan state history logChan win
    Fltk.eventLoop chan

printLogs :: LogChan -> IO ()
printLogs logChan = forever $
    Text.IO.putStrLn . Log.formatMsg
        =<< STM.atomically (TChan.readTChan logChan)

data Msg = NewLog Log.Msg | ClickedWord Text | FilterChanged Text
    deriving (Show)

handleMsgs :: Fltk.Channel -> Process.State -> Int -> LogChan
    -> LogViewC.Window -> IO ()
handleMsgs chan st history logChan win = do
    Path.AppDir appDir_ <- Path.getAppDir
    let appDir = txt appDir_ <> "/"
    flip State.evalStateT st $ forever $ do
        uiMsg <- liftIO $ getMsg logChan win
        case uiMsg of
            NewLog msg_ -> do
                let msg = stripPaths appDir msg_
                State.modify (Process.addMsg history msg)
                handleNewMsg chan win msg
            ClickedWord word -> liftIO $ handleClickedWord word
            FilterChanged expr -> do
                -- clear and redisplay msgs with new filter
                sendAction chan $ LogViewC.clearLogs win
                State.modify $ \st ->
                    st { Process.stateFilter = Process.compileFilter expr }
                allMsgs <- State.gets (reverse . Process.stateMsgs)
                mapM_ (handleNewMsg chan win) allMsgs

-- | Internally I use absolute paths, but they're pretty verbose.  So turn
-- them back into relative paths.
stripPaths :: Text -> Log.Msg -> Log.Msg
stripPaths dir msg =
    msg { Log.msgText = Text.replace dir "" $ Log.msgText msg }

handleNewMsg :: Fltk.Channel -> LogViewC.Window -> Log.Msg
    -> State.StateT Process.State IO ()
handleNewMsg chan win msg = do
    state <- State.get
    let (styled, newState) = Process.processMsg state msg
    State.put newState
    case styled of
        Just (Process.StyledText txt style) -> sendAction chan $ do
            LogViewC.appendLog win txt style
            -- This is an experimental feature to draw more attention when
            -- there's an interesting log msg.
            when (Log.msgPriority msg >= Log.Warn) $
                LogViewC.bringToFront win
        Nothing -> pure ()
    let newStatus = Process.stateStatus newState
    when (Process.stateStatus state /= newStatus) $ do
        let (Process.StyledText status style) = Process.renderStatus newStatus
        sendAction chan $ LogViewC.setStatus win status style
        State.modify $ \st -> st { Process.stateStatus = newStatus }

handleClickedWord :: Text -> IO ()
handleClickedWord word
    | "{" `Text.isPrefixOf` word && "}" `Text.isSuffixOf` word =
        sendToSeq (Text.drop 1 (Text.dropEnd 1 word))
    | otherwise = putStrLn $ "logview: unknown clicked word: " ++ show word

sendAction :: State.MonadIO m => Fltk.Channel -> Fltk.Fltk () -> m ()
sendAction chan = liftIO . Fltk.action chan

sendToSeq :: Text -> IO ()
sendToSeq cmd = do
    response <- ReplProtocol.queryCmdSimple cmd
    unless (Text.null response) $
        Text.IO.putStrLn $ "response: " <> response

getMsg :: LogChan -> LogViewC.Window -> IO Msg
getMsg logChan win = STM.atomically $
    fmap NewLog (STM.readTChan logChan)
    `STM.orElse` fmap parseUiMsg (Fltk.readMsg win)

parseUiMsg :: Fltk.Msg LogViewC.MsgType -> Msg
parseUiMsg (Fltk.Msg typ s) = case typ of
    LogViewC.Click -> ClickedWord s
    LogViewC.Command -> FilterChanged s
    LogViewC.Unknown n -> error $ "unknown msg type: " ++ show n
