{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
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
    -- * msgs
    Msg, Prio(..)
    , debug, notice, warn, error
    , debug_srcpos, notice_srcpos, warn_srcpos, error_srcpos
    -- * LogT monad
    , LogMonad
    , LogT, write, run
) where
import Prelude hiding (error, log)
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Trans as Trans
import qualified Data.Time as Time
import Text.Printf (printf)

import qualified Util.Logger as Logger
import qualified Util.Misc as Misc

data Msg = Msg
    { msg_date :: Maybe Time.UTCTime
    , msg_caller :: Misc.SrcPos
    , msg_prio :: Prio
    -- | Free form text for humans.
    , msg_text  :: String
    -- -- | Higher level context info for the msg.
    -- , msg_context :: [Context]
    } deriving (Show, Eq)

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
    deriving (Show, Enum, Eq, Ord)

-- | Create a msg with the give prio and text.
msg :: Prio -> String -> Msg
msg = msg_srcpos Nothing
msg_srcpos :: Misc.SrcPos -> Prio -> String -> Msg
msg_srcpos srcpos prio text = Msg Nothing srcpos prio text

log :: LogMonad m => Prio -> Misc.SrcPos -> String -> m ()
log prio srcpos text = write (msg_srcpos srcpos prio text)

-- The monomorphism restriction makes these signatures necessary.

debug_srcpos, notice_srcpos, warn_srcpos, error_srcpos
    :: LogMonad m => Misc.SrcPos -> String -> m ()
debug_srcpos = log Debug
notice_srcpos = log Notice
warn_srcpos = log Warn
error_srcpos = log Error

debug, notice, warn, error :: LogMonad m => String -> m ()
debug = debug_srcpos Nothing
notice = notice_srcpos Nothing
warn = warn_srcpos Nothing
error = error_srcpos Nothing

-- * LogT

class Monad m => LogMonad m where
    write :: Msg -> m ()

instance LogMonad IO where
    write msg = do
        msg' <- add_time msg
        putStrLn (default_show msg')
-- TODO show the date, if any
default_show (Msg { msg_date = date, msg_caller = srcpos, msg_prio = prio
        , msg_text = text }) =
    printf "%-4s %s- %s" (prio_stars prio) (Misc.show_srcpos srcpos) text
    where prio_stars prio = replicate (fromEnum prio + 1) '*'

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
