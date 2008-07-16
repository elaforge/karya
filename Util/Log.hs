{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{- | Functions for logging.

Logs are used to report everything from errors and debug msgs to status
reports.  They are collected in a central place that writes them to a file
in a machine-readable serialized format, and also passes them to various
registered log actions.

So a progress bar would register a function that looks for Progress msgs from
the relevant system and updates itself when it sees them.  Blocks register
a function that watches for Block errors and highlights the part of the block
that had the error (probably emitted by Derive).  Informational msgs matching
a given pattern may go to the status bar.

-}

module Util.Log (
    initialize
    -- * msgs
    , Msg(..), Prio(..)
    , msg, msg_srcpos
    , debug, notice, warn, error
    , debug_srcpos, notice_srcpos, warn_srcpos, error_srcpos
    , debug_stack, notice_stack, warn_stack, error_stack
    , debug_stack_srcpos, notice_stack_srcpos, warn_stack_srcpos
        , error_stack_srcpos
    -- * LogT monad
    , LogMonad
    , LogT, write, run
    -- * serialization
    , format_msg
    , deserialize_msg
) where
import Prelude hiding (error, log)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Trans as Trans
import qualified Data.Time as Time
import qualified Data.Typeable as Typeable
import qualified System.IO as IO
import qualified System.IO.Unsafe  as Unsafe
import Text.Printf (printf)

import qualified Util.Logger as Logger
import qualified Util.SrcPos as SrcPos

-- This import is a little iffy because Util shouldn't be importing from
-- elsewhere, but the stack uses TrackIds.  UI doesn't use the logging, so
-- I'm safe for now...
import qualified Perform.Warning as Warning


data Msg = Msg {
    msg_date :: Maybe Time.UTCTime
    , msg_caller :: SrcPos.SrcPos
    , msg_prio :: Prio
    -- | Msgs which are logged from the deriver may record the position in the
    -- schema and event being processed.
    , msg_stack :: Maybe Stack
    -- | Free form text for humans.
    , msg_text  :: String
    } deriving (Eq, Show, Typeable.Typeable, Read)

-- | One handle for the machine readable log, and one for the human readable
-- one.
data State = State (Maybe IO.Handle) (Maybe IO.Handle)
initial_state = State Nothing (Just IO.stdout)
global_state = Unsafe.unsafePerformIO (MVar.newMVar initial_state)

-- | Configure the log system to write to the given file.  Before you call
-- this, log output will go to stdout.
initialize :: IO.FilePath -> IO.FilePath -> IO ()
initialize mach_file file = do
    mach_hdl <- IO.openFile mach_file IO.AppendMode
    IO.hSetBuffering mach_hdl IO.LineBuffering
    human_hdl <- IO.openFile file IO.AppendMode
    IO.hSetBuffering human_hdl IO.LineBuffering
    MVar.swapMVar global_state (State (Just mach_hdl) (Just human_hdl))
    return ()

-- | (schema_stack, event_stack)
type Stack = [Warning.CallPos]

data Prio
    -- | Lots of msgs produced by code level.  Users don't look at this during
    -- normal use, but can be useful for debugging.
    = Debug
    -- | Informational msgs that the user will want to see.  Progress messages
    -- in e.g. derivation and play status are included here.
    | Notice
    -- | Something went wrong in e.g. derivation.  User definitely wants to see
    -- this.
    | Warn
    -- | Code error in the app, which may quit after printing this.
    | Error
    deriving (Show, Enum, Eq, Ord, Read)

-- | Create a msg with the give prio and text.
msg_srcpos :: SrcPos.SrcPos -> Prio -> Maybe Stack -> String -> Msg
msg_srcpos srcpos prio stack text = Msg Nothing srcpos prio stack text
msg :: Prio -> String -> Msg
msg prio = msg_srcpos Nothing prio Nothing

log :: LogMonad m => Prio -> SrcPos.SrcPos -> String -> m ()
log prio srcpos text = write (msg_srcpos srcpos prio Nothing text)
log_stack :: LogMonad m => Prio -> SrcPos.SrcPos -> Stack -> String -> m ()
log_stack prio srcpos stack text =
    write (msg_srcpos srcpos prio (Just stack) text)

debug_srcpos, notice_srcpos, warn_srcpos, error_srcpos
    :: LogMonad m => SrcPos.SrcPos -> String -> m ()
debug_srcpos = log Debug
notice_srcpos = log Notice
warn_srcpos = log Warn
error_srcpos = log Error

debug, notice, warn, error :: LogMonad m => String -> m ()
debug = debug_srcpos Nothing
notice = notice_srcpos Nothing
warn = warn_srcpos Nothing
error = error_srcpos Nothing

-- Yay permutation game.  I could probably do a typeclass trick to make 'stack'
-- an optional arg, but I think I'd wind up with all the same boilerplate here.
debug_stack_srcpos, notice_stack_srcpos, warn_stack_srcpos, error_stack_srcpos
    :: LogMonad m => SrcPos.SrcPos -> Stack -> String -> m ()
debug_stack_srcpos = log_stack Debug
notice_stack_srcpos = log_stack Notice
warn_stack_srcpos = log_stack Warn
error_stack_srcpos = log_stack Error

debug_stack, notice_stack, warn_stack, error_stack
    :: LogMonad m => Stack -> String -> m ()
debug_stack = debug_stack_srcpos Nothing
notice_stack = notice_stack_srcpos Nothing
warn_stack = warn_stack_srcpos Nothing
error_stack = error_stack_srcpos Nothing

-- * LogT

class Monad m => LogMonad m where
    write :: Msg -> m ()

maybe_do m maybe_val = maybe (return ()) id (fmap m maybe_val)

instance LogMonad IO where
    write msg = do
        msg' <- add_time msg
        MVar.withMVar global_state $ \(State mach_hdl human_hdl) -> do
            maybe_do (flip IO.hPutStrLn (serialize_msg msg')) mach_hdl
            maybe_do (flip IO.hPutStrLn (format_msg msg')) human_hdl
-- TODO show the date, if any
format_msg :: Msg -> String
format_msg (Msg { msg_date = _date, msg_caller = srcpos, msg_prio = prio
        , msg_text = text, msg_stack = stack }) =
    msg ++ maybe "" ((++" ") . show) stack
    where
    prio_stars prio = replicate (fromEnum prio + 1) '*'
    msg = printf "%-4s %s- %s"
        (prio_stars prio) (SrcPos.show_srcpos srcpos) text

-- | Add a time to the msg if it doesn't already have one.  Msgs can be logged
-- outside of IO, so they don't get a date until they are written.
add_time :: Msg -> IO Msg
add_time msg = case msg_date msg of
    Nothing -> do
        utc <- Time.getCurrentTime
        return $ msg {msg_date = Just utc}
    Just _ -> return msg

instance Monad m => LogMonad (LogT m) where
    write msg = write_msg msg

write_msg :: Monad m => Msg -> LogT m ()
write_msg = LogT . Logger.record

type LogM m = Logger.LoggerT Msg m
newtype Monad m => LogT m a = LogT (LogM m a)
    deriving (Functor, Monad, Trans.MonadIO, Trans.MonadTrans,
        Error.MonadError e)
run_log_t (LogT x) = x

run :: Monad m => LogT m a -> m (a, [Msg])
run = Logger.run . run_log_t

-- * serialize

serialize_msg :: Msg -> String
serialize_msg = show
deserialize_msg :: String -> IO (Either Exception.Exception Msg)
deserialize_msg msg = Exception.try (readIO msg)


-- unused

{-
-- system could be dynamically scoped, 'Log.with_system sys (code)'
-- but only if I stuck a State into LogT, and wouldn't work for IO logs
data System
    = App -- ^ app level stuff, goes to stderr
    | Playback
    | Derive
    | UI
    deriving (Show, Eq)

-- | More specific data for the msg.  A derivation would send Progress
-- reports at Info, an error in derivation would send Block at Warn, and
-- playback would send Block at Info.
data Context
    -- | Progress report on a process.
    = Progress
        { progress_amount :: Double
        , progress_total :: Double
        }
    -- | Msg pertains to this region in a block.
    | Block
        { block_name :: String -- ^ Just the name, to avoid circular imports.
        , block_area :: () -- Types.Selection -- ^ affected area
        }
    deriving (Show, Eq)

-}
