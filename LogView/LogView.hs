{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | This module reads lines and streams them to the GUI, which displays them
    in a scrolling box.  Clickable text is marked and will be highlighted in
    the GUI.  When it's clicked, the GUI sends the tag back, much like an HTML
    href.

    This also maintains a filter.  It to the GUI for a simple language that
    modifies the filter.  When the filter is changed, it refilters the msgs,
    clears the GUI, and sends the new msgs over.
-}
module LogView.LogView where
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Monad
import qualified Control.Monad.State as State
import Control.Monad.State (liftIO)
import qualified System.Environment
import qualified System.Exit
import qualified System.Console.GetOpt as GetOpt

import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified LogView.LogViewC as LogViewC
import qualified LogView.Process as Process


mach_log_filename :: String
mach_log_filename = "seq.mach.log"

-- | Initial contents of the filter field.
initial_filter :: String
initial_filter = "**"

initial_size :: (Int, Int)
initial_size = (500, 700)

default_catch_patterns :: [Process.CatchPattern]
default_catch_patterns =
    [ ("octave", Process.make_regex "octave: ([0-9]+)")
    ]


data Flag = Help | NoSeek deriving (Eq, Show)
options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["help"] (GetOpt.NoArg Help) "display usage"
    , GetOpt.Option [] ["noseek"] (GetOpt.NoArg NoSeek) "don't seek to end"
    ]
usage msg = putStr (GetOpt.usageInfo msg options) >> System.Exit.exitFailure

main :: IO ()
main = do
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (opts, n, []) -> return (opts, n)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    when (not (null args)) $
        usage ("unparsed args: " ++ show args)
    when (Help `elem` flags) (usage "usage:")

    let seek = NoSeek `notElem` flags

    view <- LogViewC.create_logview 20 20 (fst initial_size) (snd initial_size)
    LogViewC.set_filter view initial_filter

    log_chan <- STM.newTChanIO
    Concurrent.forkIO (Process.tail_file log_chan mach_log_filename seek)
    let state = (Process.initial_state initial_filter)
            { Process.state_catch_patterns = default_catch_patterns }
    Concurrent.forkIO (handle_msgs state log_chan view)
    LogViewC.run


data Msg = NewLog Log.Msg | ClickedWord String | FilterChanged String
    deriving (Show)

handle_msgs :: Process.State -> STM.TChan Log.Msg -> LogViewC.LogView -> IO ()
handle_msgs state log_chan view = flip State.evalStateT state $ forever $ do
    msg <- liftIO $ get_msg log_chan view
    case msg of
        NewLog log -> do
            State.modify $ \st ->
                st { Process.state_msgs = log : Process.state_msgs st }
            handle_new_msg view log
        ClickedWord word -> do
            liftIO $ putStrLn $ "clicked: " ++ show word
        FilterChanged expr -> do
            -- clear and redisplay msgs with new filter
            send $ LogViewC.clear_logs view
            State.modify $ \st ->
                st { Process.state_filter = Process.compile_filter expr }
            all_msgs <- fmap (reverse . Process.state_msgs) State.get
            mapM_ (handle_new_msg view) all_msgs

handle_new_msg view msg = do
    state <- State.get
    let (new_state, styled) = Process.process_msg state msg
    State.put new_state
    case styled of
        Just styled -> let (log_s, style_s) = Process.extract_style styled
            in send $ LogViewC.append_log view log_s style_s
        Nothing -> return ()
    let new_status = Process.state_status new_state
    when (Process.state_status state /= new_status) $ do
        send $ LogViewC.set_status view (Process.render_status new_status)
        State.modify $ \st -> st { Process.state_status = new_status }

send act = liftIO (LogViewC.send_action act)

get_msg log_chan view = STM.atomically $
    fmap NewLog (STM.readTChan log_chan)
    `STM.orElse` fmap parse_ui_msg (LogViewC.read_msg view)

parse_ui_msg (typ, s) = case typ of
    LogViewC.Click -> ClickedWord s
    LogViewC.Command -> FilterChanged s
    LogViewC.Unknown n -> error $ "unknown msg type: " ++ show n
