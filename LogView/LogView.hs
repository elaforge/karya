{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{- | This module reads lines and streams them to the GUI, which displays them
    in a scrolling box.  Clickable text is marked and will be highlighted in
    the GUI.  When it's clicked, the GUI sends the tag back, much like an HTML
    href.

    This also maintains a filter.  The filter is a custom little language that
    will filter out messages that don't match.

    In addition, there is a concept of "catch patterns".  These are regexes
    which are matched against msg text.  When one matches, the matched groups
    are kept in a status line.  That way, events reported in the log can be
    collected together.
-}
module LogView.LogView where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.State as State
import Control.Monad.Trans (liftIO)

import qualified Data.List as List
import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.Exit
import System.FilePath ((</>))

import qualified Util.Fltk as Fltk
import qualified Util.Log as Log
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq

import qualified LogView.LogViewC as LogViewC
import qualified LogView.Process as Process
import qualified App.Config as Config
import qualified App.SendCmd as SendCmd


-- | Initial contents of the filter field.
initial_filter :: String
initial_filter = "**"

initial_size :: (Int, Int)
initial_size = (900, 300)

default_catch_patterns :: [Process.CatchPattern]
default_catch_patterns =
    [ ("_", Regex.make "^global status: (.*?) -- (.*)")
    -- I wound up having the app emit catch patterns explicitly instead of
    -- putting the smarts in logview.  But it still seems conceivable that
    -- I may want logview to catch things the app didn't explicitly mean to
    -- be caught, so I'll leave this functionality in.
    ]

-- | Remember this many log msgs.
default_history :: Int
default_history = 1000

-- | UI will remember this many bytes.  This is not the same as
-- 'default_history' because the history will remember filtered out msgs, and
-- the UI doesn't bother to preserve msg boundaries so it uses bytes.
default_max_bytes :: Int
default_max_bytes = default_history * 100

data Flag = Help | Seek (Maybe Integer) | Print | History Int | File String
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

main :: IO ()
main = do
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    unless (null args) $
        usage ("unparsed args: " ++ show args)
    when (Help `elem` flags) (usage "usage:")

    let seek = maybe (Just 0) id $ Seq.last [s | Seek s <- flags]
        history = maybe default_history id $ Seq.last [n | History n <- flags]
    filename <- maybe default_log_fn return $ Seq.last [n | File n <- flags]
    hdl <- Process.open filename seek
    log_chan <- STM.newTChanIO
    Concurrent.forkIO (Process.tail_file log_chan filename hdl)
    if Print `elem` flags
        then print_logs log_chan
        else gui log_chan filename history
    where
    usage msg = do
        putStrLn "usage: logview [ flags ]"
        putStr (GetOpt.usageInfo msg options)
        System.Exit.exitFailure

default_log_fn :: IO FilePath
default_log_fn = Config.get_app_dir >>= \app_dir -> return $
    Config.make_path app_dir Config.log_dir </> "seq.log"

gui :: STM.TChan Log.Msg -> FilePath -> Int -> IO ()
gui log_chan filename history = do
    filename <- Directory.canonicalizePath filename
    win <- LogViewC.create 20 20 (fst initial_size) (snd initial_size)
        filename default_max_bytes
    LogViewC.set_filter win initial_filter
    let state = (Process.initial_state initial_filter)
            { Process.state_catch_patterns = default_catch_patterns }
    Concurrent.forkIO (handle_msgs state history log_chan win)
    Fltk.run

print_logs :: STM.TChan Log.Msg -> IO ()
print_logs log_chan = forever $ do
    msg <- STM.atomically $ STM.readTChan log_chan
    putStrLn $ Log.format_msg msg


data Msg = NewLog Log.Msg | ClickedWord String | FilterChanged String
    deriving (Show)

handle_msgs :: Process.State -> Int -> STM.TChan Log.Msg -> LogViewC.Window
    -> IO ()
handle_msgs st history log_chan win = flip State.evalStateT st $ forever $ do
    msg <- liftIO $ get_msg log_chan win
    case msg of
        NewLog log_msg -> do
            State.modify (Process.add_msg history log_msg)
            handle_new_msg win log_msg
        ClickedWord word -> liftIO $ handle_clicked_word word
        FilterChanged expr -> do
            -- clear and redisplay msgs with new filter
            send_action $ LogViewC.clear_logs win
            State.modify $ \st ->
                st { Process.state_filter = Process.compile_filter expr }
            all_msgs <- State.gets (reverse . Process.state_msgs)
            mapM_ (handle_new_msg win) all_msgs

handle_new_msg :: LogViewC.Window -> Log.Msg
    -> State.StateT Process.State IO ()
handle_new_msg win msg = do
    state <- State.get
    let (styled, new_state) = Process.process_msg state msg
    State.put new_state
    case styled of
        Just styled -> let (log_s, style_s) = Process.extract_style styled
            in send_action $ LogViewC.append_log win log_s style_s
        Nothing -> return ()
    let new_status = Process.state_status new_state
    when (Process.state_status state /= new_status) $ do
        let (Process.StyledText status style) = Process.render_status new_status
        send_action $ LogViewC.set_status win status style
        State.modify $ \st -> st { Process.state_status = new_status }

handle_clicked_word :: String -> IO ()
handle_clicked_word word
    | "{" `List.isPrefixOf` word && "}" `List.isSuffixOf` word =
        send_to_app (drop 1 (Seq.rdrop 1 word))
    | otherwise = putStrLn $ "unknown clicked word: " ++ show word

send_action :: (State.MonadIO m) => IO a -> m ()
send_action = liftIO . Fltk.send_action

send_to_app :: String -> IO ()
send_to_app cmd = do
    response <- SendCmd.send cmd
        `Exception.catch` \(exc :: Exception.SomeException) ->
            return ("error: " ++ show exc)
    unless (null response) $
        putStrLn $ "response: " ++ response

get_msg :: STM.TChan Log.Msg -> LogViewC.Window -> IO Msg
get_msg log_chan win = STM.atomically $
    fmap NewLog (STM.readTChan log_chan)
    `STM.orElse` fmap parse_ui_msg (Fltk.read_msg win)

parse_ui_msg :: Fltk.Msg LogViewC.MsgType -> Msg
parse_ui_msg (Fltk.Msg typ s) = case typ of
    LogViewC.Click -> ClickedWord s
    LogViewC.Command -> FilterChanged s
    LogViewC.Unknown n -> error $ "unknown msg type: " ++ show n
