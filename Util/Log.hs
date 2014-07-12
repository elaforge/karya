-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, BangPatterns #-}
{- | Functions for logging.

    Log msgs are used to report everything from errors and debug msgs to status
    reports.  They are collected in a central place that writes them to a file
    in a machine-readable serialized format.
-}

module Util.Log (
    configure
    -- * msgs
    , Msg(..), Stack, msg_string, Prio(..), State(..)
    , msg, msg_srcpos, initialized_msg, initialized_msg_srcpos
    , timer, debug, notice, warn, error
    , timer_srcpos, debug_srcpos, notice_srcpos, warn_srcpos, error_srcpos
    , debug_stack, notice_stack, warn_stack, error_stack
    , debug_stack_srcpos, notice_stack_srcpos, warn_stack_srcpos
        , error_stack_srcpos
    , add_prefix
    , trace_logs
    -- * LogT monad
    , LogMonad(..)
    , LogT, run
    , format_msg
    , serialize_msg, deserialize_msg

    -- * util
    , time_eval
) where
import Prelude hiding (error, log)
import qualified Control.Applicative as Applicative
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State.Strict
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Writer.Lazy as Writer

import qualified Data.Generics as Generics
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Time as Time

import qualified System.CPUTime as CPUTime
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import Text.Printf (printf)

import Util.Control
import qualified Util.Debug as Debug
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.SrcPos as SrcPos


type Stack = [String]

data Msg = Msg {
    msg_date :: !Time.UTCTime
    , msg_caller :: !SrcPos.SrcPos
    , msg_priority :: !Prio
    -- | Msgs which are logged from the deriver may record the position in the
    -- score the msg was emitted.
    , msg_stack :: !(Maybe Stack)
    -- | Free form text for humans.
    , msg_text :: !Text
    } deriving (Eq, Show, Read, Generics.Typeable)

instance DeepSeq.NFData Msg where
    rnf (Msg {}) = ()

msg_string :: Msg -> String
msg_string = Text.unpack . msg_text

instance Pretty.Pretty Msg where
    pretty = format_msg

-- | Pure code can't give a date, but making msg_date Maybe makes it awkward
-- for everyone who processes Msgs, so cheat with this.
no_date_yet :: Time.UTCTime
no_date_yet = Time.UTCTime (Time.ModifiedJulianDay 0) 0

-- | Logging state.  Don't log if a handle is Nothing.
data State = State {
    state_log_hdl :: Maybe IO.Handle
    , state_log_level :: Prio
    -- | Function to format a Msg for output.
    -- TODO it could be Text, but there's not much point as long as the
    -- default serialize and deserialize is show and read.
    , state_log_formatter :: Msg -> String
    }

initial_state :: State
initial_state = State
    { state_log_hdl = Just IO.stderr
    , state_log_level = Debug
    , state_log_formatter = format_msg
    }

{-# NOINLINE global_state #-}
global_state :: MVar.MVar State
global_state = Unsafe.unsafePerformIO (MVar.newMVar initial_state)

-- | Configure the logging system by modifying its internal state.
-- Return the old state so you can restore it later.
configure :: (State -> State) -> IO State
configure f = MVar.modifyMVar global_state $ \old -> do
    let new = f old
    -- This seems like a bit of a hack, but it does seem like the hdl should be
    -- in line mode.
    case state_log_hdl new of
        Just hdl -> IO.hSetBuffering hdl IO.LineBuffering
        _ -> return ()
    return (new, new)

data Prio =
    -- | Logs to determine where things are hanging when debugging
    -- a performance problem.  Use "LogView.ShowTimers" to show the time
    -- elapsed between Timer logs.
    Timer
    -- | Lots of msgs produced by code level.  Users don't look at this during
    -- normal use, but can be useful for debugging.
    | Debug
    -- | Informational msgs that the user will want to see.  Progress messages
    -- in e.g. derivation and play status are included here.
    | Notice
    -- | Something went wrong in e.g. derivation.  User definitely wants to see
    -- this.
    | Warn
    -- | Code error in the app, which may quit after printing this.
    | Error
    deriving (Show, Enum, Eq, Ord, Read)

-- | Create a msg without initializing it, so it doesn't have to be in
-- LogMonad.
msg :: Prio -> Maybe Stack -> Text -> Msg
msg = msg_srcpos Nothing

-- | Create a msg without initializing it.
msg_srcpos :: SrcPos.SrcPos -> Prio -> Maybe Stack -> Text -> Msg
msg_srcpos = Msg no_date_yet

-- | Create a msg with the give prio and text.
initialized_msg_srcpos :: (LogMonad m) => SrcPos.SrcPos -> Prio -> Text -> m Msg
initialized_msg_srcpos srcpos prio = make_msg srcpos prio Nothing

initialized_msg :: (LogMonad m) => Prio -> Text -> m Msg
initialized_msg = initialized_msg_srcpos Nothing

-- | This is the main way to construct a Msg since 'initialize_msg' is called.
make_msg :: (LogMonad m) =>
    SrcPos.SrcPos -> Prio -> Maybe Stack -> Text -> m Msg
make_msg srcpos prio stack text =
    initialize_msg (msg_srcpos srcpos prio stack text)

log :: (LogMonad m) => Prio -> SrcPos.SrcPos -> String -> m ()
log prio srcpos text = write =<< make_msg srcpos prio Nothing (txt text)

log_stack :: (LogMonad m) => Prio -> SrcPos.SrcPos -> Stack -> String
    -> m ()
log_stack prio srcpos stack text =
    write =<< make_msg srcpos prio (Just stack) (txt text)

timer_srcpos, debug_srcpos, notice_srcpos, warn_srcpos, error_srcpos
    :: (LogMonad m) => SrcPos.SrcPos -> String -> m ()
timer_srcpos = log Timer
debug_srcpos = log Debug
notice_srcpos = log Notice
warn_srcpos = log Warn
error_srcpos = log Error

timer, debug, notice, warn, error :: (LogMonad m) => String -> m ()
timer = timer_srcpos Nothing
debug = debug_srcpos Nothing
notice = notice_srcpos Nothing
warn = warn_srcpos Nothing
error = error_srcpos Nothing

-- Yay permutation game.  I could probably do a typeclass trick to make 'stack'
-- an optional arg, but I think I'd wind up with all the same boilerplate here.
debug_stack_srcpos, notice_stack_srcpos, warn_stack_srcpos, error_stack_srcpos
    :: (LogMonad m) => SrcPos.SrcPos -> Stack -> String -> m ()
debug_stack_srcpos = log_stack Debug
notice_stack_srcpos = log_stack Notice
warn_stack_srcpos = log_stack Warn
error_stack_srcpos = log_stack Error

debug_stack, notice_stack, warn_stack, error_stack :: (LogMonad m) =>
    Stack -> String -> m ()
debug_stack = debug_stack_srcpos Nothing
notice_stack = notice_stack_srcpos Nothing
warn_stack = warn_stack_srcpos Nothing
error_stack = error_stack_srcpos Nothing

-- | Prefix msgs with the given string.
add_prefix :: Text -> [Msg] -> [Msg]
add_prefix pref = map $ \m -> m { msg_text = pref <> ": " <> msg_text m }

-- | Write log msgs with 'trace', for debugging.
trace_logs :: [Msg] -> a -> a
trace_logs logs val
    | null logs = val
    | otherwise = Debug.trace_str
        (Seq.rdrop_while (=='\n') $ unlines $ "\tlogged:" : map format_msg logs)
        val

-- * LogT

class (Monad m) => LogMonad m where
    write :: Msg -> m ()

    -- | An instance for whatever monad you're using can use this to add some
    -- custom data, like stack information.
    initialize_msg :: Msg -> m Msg
    initialize_msg = return

instance LogMonad IO where
    write log_msg = do
        -- This is also done by 'initialize_msg', but if the msg was created
        -- outside of IO, it won't have had IO's 'initialize_msg' run on it.
        MVar.withMVar global_state $ \(State m_hdl prio formatter) ->
            case m_hdl of
                Just hdl | prio <= msg_priority log_msg -> do
                    -- Go to a little bother to only run 'add_time' for msgs
                    -- that are actually logged.
                    log_msg <- add_time log_msg
                    IO.hPutStrLn hdl (formatter log_msg)
                _ -> return ()
        when (msg_priority log_msg == Error) $ do
            log_msg <- add_time log_msg
            IO.hPutStrLn IO.stderr (format_msg log_msg)

-- | Format a msg in a nice user readable way.
format_msg :: Msg -> String
format_msg (Msg { msg_date = _date, msg_caller = srcpos, msg_priority = prio
        , msg_text = text, msg_stack = stack }) =
    log_msg ++ maybe "" ((' ':) . Seq.join " / ") stack
    where
    prio_stars Timer = "-"
    prio_stars prio = replicate (fromEnum prio) '*'
    log_msg = printf "%-4s %s - %s"
        (prio_stars prio) (SrcPos.show_srcpos srcpos) (Text.unpack text)

-- | Add a time to the msg if it doesn't already have one.  Msgs can be logged
-- outside of IO, so they don't get a date until they are written.
add_time :: Msg -> IO Msg
add_time log_msg
    | msg_date log_msg == no_date_yet = do
        utc <- Time.getCurrentTime
        return $ log_msg { msg_date = utc }
    | otherwise = return log_msg

instance (Monad m) => LogMonad (LogT m) where
    write = write_msg

write_msg :: Monad m => Msg -> LogT m ()
write_msg = LogT . Logger.log

type LogM m = Logger.LoggerT Msg m
newtype LogT m a = LogT (LogM m a)
    deriving (Functor, Monad, Trans.MonadIO, Trans.MonadTrans,
        Error.MonadError e, State.MonadState st, Reader.MonadReader r)
run_log_t (LogT x) = x

instance (Functor m, Monad m) => Applicative.Applicative (LogT m) where
    pure = return
    (<*>) = ap

run :: Monad m => LogT m a -> m (a, [Msg])
run = Logger.run . run_log_t

-- ** mtl instances

instance LogMonad m => LogMonad (State.Strict.StateT s m) where
    write = Trans.lift . write
instance LogMonad m => LogMonad (State.Lazy.StateT s m) where
    write = Trans.lift . write
instance (Error.Error e, LogMonad m) => LogMonad (Error.ErrorT e m) where
    write = Trans.lift . write
instance LogMonad m => LogMonad (Reader.ReaderT r m) where
    write = Trans.lift . write
instance (Monoid.Monoid w, LogMonad m) => LogMonad (Writer.WriterT w m) where
    write = Trans.lift . write

-- * serialize

serialize_msg :: Msg -> String
serialize_msg = show

deserialize_msg :: String -> IO (Either Exception.SomeException Msg)
deserialize_msg log_msg = Exception.try (readIO log_msg)

-- * util

-- | Run an action and report the time in CPU seconds.
time_eval :: IO a -> IO (a, Double)
time_eval op = do
    start_cpu <- CPUTime.getCPUTime
    !val <- op
    end_cpu <- CPUTime.getCPUTime
    return (val, cpu_to_sec (end_cpu - start_cpu))

cpu_to_sec :: Integer -> Double
cpu_to_sec s = fromIntegral s / fromIntegral (10^12)
