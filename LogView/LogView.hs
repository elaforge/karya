-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
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

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.Posix as Posix

import qualified Util.File as File
import qualified Util.Fltk as Fltk
import qualified Util.Log as Log
import qualified Util.Process
import qualified Util.Seq as Seq

import qualified LogView.LogViewC as LogViewC
import qualified LogView.Process as Process
import qualified LogView.Tail as Tail

import qualified App.ReplProtocol as ReplProtocol

import Global


-- | I use this file to only start one logview at a time.
pid_file :: FilePath
pid_file = "log/logview.pid"

-- | Initial contents of the filter field.
initial_filter :: Text
initial_filter = "**"

initial_size :: (Int, Int)
initial_size = (900, 300)

-- | Built-in list of catch patterns.
--
-- I wound up having the app emit catch patterns explicitly instead of putting
-- the smarts in logview, so now the only CatchPattern is
-- 'Process.global_status_pattern'.  But it still seems conceivable that
-- someday I may want logview to catch things the app didn't explicitly mean to
-- be caught, so I'll leave this functionality in for now.
default_catch_patterns :: [Process.CatchPattern]
default_catch_patterns = [Process.global_status_pattern]

-- | Remember this many log msgs.
default_history :: Int
default_history = 1000

-- | UI will remember this many bytes.  This is not the same as
-- 'default_history' because the history will remember filtered out msgs, and
-- the UI doesn't bother to preserve msg boundaries so it uses bytes.
default_max_bytes :: Int
default_max_bytes = default_history * 100

data Flag = Help | Seek (Maybe Integer) | Print | History Int | File FilePath
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
        (GetOpt.ReqArg (History . read) (show default_history))
        "remember this many lines"
    , GetOpt.Option [] ["file"] (GetOpt.ReqArg File "seq.log")
        "read from this file"
    ]

type LogChan = TChan.TChan Log.Msg

main :: IO ()
main = do
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    unless (null args) $
        usage ("unparsed args: " ++ show args)
    when (Help `elem` flags) (usage "usage:")
    whenJustM write_pid $ \pid -> do
        putStrLn $ "logview already running with pid " ++ show pid
        System.Exit.exitSuccess
    logview flags `Exception.finally` Directory.removeFile pid_file
    where
    usage msg = do
        putStrLn "usage: logview [ flags ]"
        putStr (GetOpt.usageInfo msg options)
        System.Exit.exitFailure

logview :: [Flag] -> IO ()
logview flags = do
    let seek = fromMaybe (Just 0) $ Seq.last [s | Seek s <- flags]
        history = fromMaybe default_history $ Seq.last [n | History n <- flags]
    filename <- maybe Tail.log_filename return $ Seq.last [n | File n <- flags]
    hdl <- Tail.open filename seek
    log_chan <- STM.newTChanIO
    gui_chan <- if Print `elem` flags then return Nothing
        else Just <$> Fltk.new_channel
    main_thread <- Concurrent.myThreadId
    Concurrent.forkFinally (tail_loop log_chan hdl) $ \result -> do
        putStrLn $ "tail loop died: " ++ either show (const "no exception")
            (result :: Either Exception.SomeException ())
        maybe (Concurrent.killThread main_thread) Fltk.quit gui_chan
    case gui_chan of
        Nothing -> print_logs log_chan
        Just chan -> gui chan log_chan filename history
    where
    tail_loop log_chan hdl = do
        (msg, hdl) <- Tail.tail hdl
        STM.atomically $ TChan.writeTChan log_chan msg
        tail_loop log_chan hdl

-- | If a logview is already running, return its pid, otherwise write the
-- current pid.
write_pid :: IO (Maybe Posix.ProcessID)
write_pid = do
    -- I have to use ByteString.readFile and writeFile to avoid GHC's obnoxious
    -- file locking.
    pid_str <- File.ignoreEnoent $ ByteString.readFile pid_file
    existing <- case ByteString.readInt =<< pid_str of
        Just (pid_, _) -> do
            maybe_cmd <- Util.Process.commandName pid
            return $ if maybe False ("logview" `List.isSuffixOf`) maybe_cmd
                then Just pid else Nothing
            where pid = fromIntegral pid_
        _ -> return Nothing
    when (existing == Nothing) $ do
        pid <- Posix.getProcessID
        ByteString.writeFile pid_file (ByteString.pack (show pid) <> "\n")
    return existing

gui :: Fltk.Channel -> LogChan -> FilePath -> Int -> IO ()
gui chan log_chan filename history = do
    filename <- Directory.canonicalizePath filename
    win <- Fltk.run_action $
        LogViewC.create 20 20 (fst initial_size) (snd initial_size)
            filename default_max_bytes
    Fltk.run_action $ LogViewC.set_filter win initial_filter
    let state = (Process.initial_state initial_filter)
            { Process.state_catch_patterns = default_catch_patterns }
    Concurrent.forkIO $ handle_msgs chan state history log_chan win
    Fltk.event_loop chan

print_logs :: LogChan -> IO ()
print_logs log_chan = forever $
    Text.IO.putStrLn . Log.format_msg
        =<< STM.atomically (TChan.readTChan log_chan)

data Msg = NewLog Log.Msg | ClickedWord Text | FilterChanged Text
    deriving (Show)

handle_msgs :: Fltk.Channel -> Process.State -> Int -> LogChan
    -> LogViewC.Window -> IO ()
handle_msgs chan st history log_chan win =
    flip State.evalStateT st $ forever $ do
        msg <- liftIO $ get_msg log_chan win
        case msg of
            NewLog log_msg -> do
                State.modify (Process.add_msg history log_msg)
                handle_new_msg chan win log_msg
            ClickedWord word -> liftIO $ handle_clicked_word word
            FilterChanged expr -> do
                -- clear and redisplay msgs with new filter
                send_action chan $ LogViewC.clear_logs win
                State.modify $ \st ->
                    st { Process.state_filter = Process.compile_filter expr }
                all_msgs <- State.gets (reverse . Process.state_msgs)
                mapM_ (handle_new_msg chan win) all_msgs

handle_new_msg :: Fltk.Channel -> LogViewC.Window -> Log.Msg
    -> State.StateT Process.State IO ()
handle_new_msg chan win msg = do
    state <- State.get
    let (styled, new_state) = Process.process_msg state msg
    State.put new_state
    case styled of
        Just (Process.StyledText txt style) -> send_action chan $
            LogViewC.append_log win txt style
        Nothing -> return ()
    let new_status = Process.state_status new_state
    when (Process.state_status state /= new_status) $ do
        let (Process.StyledText status style) = Process.render_status new_status
        send_action chan $ LogViewC.set_status win status style
        State.modify $ \st -> st { Process.state_status = new_status }

handle_clicked_word :: Text -> IO ()
handle_clicked_word word
    | "{" `Text.isPrefixOf` word && "}" `Text.isSuffixOf` word =
        send_to_app (Text.drop 1 (Text.dropEnd 1 word))
    | otherwise = putStrLn $ "logview: unknown clicked word: " ++ show word

send_action :: State.MonadIO m => Fltk.Channel -> Fltk.Fltk () -> m ()
send_action chan = liftIO . Fltk.send_action chan

send_to_app :: Text -> IO ()
send_to_app cmd =
    Text.IO.putStrLn . ("response: "<>) =<< ReplProtocol.query_cmd cmd

get_msg :: LogChan -> LogViewC.Window -> IO Msg
get_msg log_chan win = STM.atomically $
    fmap NewLog (STM.readTChan log_chan)
    `STM.orElse` fmap parse_ui_msg (Fltk.read_msg win)

parse_ui_msg :: Fltk.Msg LogViewC.MsgType -> Msg
parse_ui_msg (Fltk.Msg typ s) = case typ of
    LogViewC.Click -> ClickedWord s
    LogViewC.Command -> FilterChanged s
    LogViewC.Unknown n -> error $ "unknown msg type: " ++ show n
