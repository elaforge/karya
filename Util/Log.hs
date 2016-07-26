-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- Monad.Error
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, BangPatterns #-}
{-# LANGUAGE ImplicitParams, ConstraintKinds #-}
{- | Functions for logging.

    Log msgs are used to report everything from errors and debug msgs to status
    reports.  They are collected in a central place that writes them to a file
    in a machine-readable serialized format.
-}
module Util.Log (
    configure
    -- * msgs
    , Msg(..), msg_string
    , Data(..), empty_data, has_data, get_data
    , Prio(..), State(..)
    , write_json, write_formatted
    , msg, msg_call_stack, initialized_msg
    , timer, debug, notice, warn, error
    , debug_stack, notice_stack, warn_stack, error_stack
    , debug_data
    , add_prefix
    , trace_logs
    -- * LogT monad
    , LogMonad(..)
    , LogT, run
    , format_msg
    , serialize, deserialize

    -- * util
    , time_eval, format_time
) where
import Prelude hiding (error, log)
import qualified Control.Applicative as Applicative
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State.Strict
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Writer.Lazy as Writer

import qualified Data.Aeson as Aeson
import Data.Aeson (parseJSON, toJSON)
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Dynamic as Dynamic
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified GHC.Generics as Generics
import qualified GHC.Stack
import qualified System.CPUTime as CPUTime
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.CallStack as CallStack
import qualified Util.Debug as Debug
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty

import qualified Derive.Stack as Stack
import Global


data Msg = Msg {
    msg_date :: !Time.UTCTime
    , msg_caller :: !CallStack.Caller
    , msg_priority :: !Prio
    -- | Msgs which are logged from the deriver may record the position in the
    -- score the msg was emitted.
    , msg_stack :: !(Maybe Stack.Stack)
    -- | Free form text for humans.
    , msg_text :: !Text
    , msg_data :: !Data
    } deriving (Eq, Show, Read)

instance DeepSeq.NFData Msg where
    rnf (Msg {}) = ()

msg_string :: Msg -> String
msg_string = Text.unpack . msg_text

instance Pretty.Pretty Msg where
    pretty = format_msg

-- | Attach an arbitrary payload to a log msg.
--
-- Since Dynamic can't preserve its value across (read . show), the Show just
-- has the type, not the value.  To avoid being confused by putting that back
-- into Data as a String, it then becomes StrippedData to make it clear what
-- happened.  Overkill?  Probably.
data Data = NoData | Data !Dynamic.Dynamic | StrippedData !String

empty_data :: Data
empty_data = Data (Dynamic.toDyn ())

has_data :: Msg -> Bool
has_data msg = case msg_data msg of
    NoData -> False
    _ -> True

get_data :: Dynamic.Typeable a => Msg -> Maybe a
get_data msg = case msg_data msg of
    Data d -> Dynamic.fromDynamic d
    _ -> Nothing

instance Eq Data where
    NoData == NoData = True
    StrippedData t1 == StrippedData t2 = t1 == t2
    _ == _ = False

instance Read.Read Data where readPrec = Read.lift read_data
instance Show Data where
    show NoData = "NoData"
    show (Data d) =
        "Data " ++ show (filter (/='"') (show (Dynamic.dynTypeRep d)))
    show (StrippedData typ) = "StrippedData " ++ show typ

read_data :: ReadP.ReadP Data
read_data = ReadP.choice
    [ ReadP.string "NoData" *> pure NoData
    , StrippedData <$> (ReadP.string "Data" *> p_string)
    , StrippedData <$> (ReadP.string "StrippedData" *> p_string)
    ]
    where
    -- I stripped out "s in Show, so this should be good enough.
    p_string = ReadP.skipSpaces *>
        ReadP.between (ReadP.string "\"") (ReadP.string "\"")
            (ReadP.munch (/='"'))

-- | Pure code can't give a date, but making msg_date Maybe makes it awkward
-- for everyone who processes Msgs, so cheat with this.
no_date_yet :: Time.UTCTime
no_date_yet = Time.UTCTime (Time.ModifiedJulianDay 0) 0

-- | Logging state.  Don't log if a handle is Nothing.
data State = State {
    state_write_msg :: Msg -> IO ()
    , state_log_level :: Prio
    }

-- | Write logs as JSON to the given handle.
write_json :: IO.Handle -> Msg -> IO ()
write_json hdl log_msg = do
    ByteString.Lazy.hPut hdl (serialize log_msg)
    ByteString.Lazy.hPut hdl "\n"

-- | Write logs as human-readable text.
write_formatted :: IO.Handle -> Msg -> IO ()
write_formatted hdl = Text.IO.hPutStrLn hdl . format_msg

initial_state :: State
initial_state = State
    { state_write_msg = write_formatted IO.stderr
    , state_log_level = Debug
    }

{-# NOINLINE global_state #-}
global_state :: MVar.MVar State
global_state = Unsafe.unsafePerformIO (MVar.newMVar initial_state)

-- | Configure the logging system by modifying its internal state.
-- Return the old state so you can restore it later.
configure :: (State -> State) -> IO State
configure f = MVar.modifyMVar global_state $ \old -> return (f old, old)

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
    deriving (Bounded, Enum, Show, Read, Eq, Ord, Generics.Generic)

-- | Create a msg without initializing it, so it doesn't have to be in
-- LogMonad.
msg :: CallStack.Stack => Prio -> Maybe Stack.Stack -> Text -> Msg
msg = msg_call_stack ?stack

-- | Like 'msg' but when you already have a CallStack.
msg_call_stack :: GHC.Stack.CallStack -> Prio -> Maybe Stack.Stack -> Text
    -> Msg
msg_call_stack call_stack prio stack text =
    Msg no_date_yet (CallStack.caller call_stack) prio stack text NoData

-- | Create a msg with the given prio and text.
initialized_msg :: (CallStack.Stack, LogMonad m) => Prio -> Text -> m Msg
initialized_msg prio = make_msg prio Nothing

-- | This is the main way to construct a Msg since 'initialize_msg' is called.
make_msg :: (CallStack.Stack, LogMonad m) => Prio -> Maybe Stack.Stack -> Text
    -> m Msg
make_msg prio stack text = initialize_msg (msg prio stack text)

log :: (CallStack.Stack, LogMonad m) => Prio -> Text -> m ()
log prio text = write =<< make_msg prio Nothing text

log_stack :: (CallStack.Stack, LogMonad m) => Prio -> Stack.Stack -> Text
    -> m ()
log_stack prio stack text = write =<< make_msg prio (Just stack) text

timer, debug, notice, warn, error
    :: (CallStack.Stack, LogMonad m) => Text -> m ()
timer = log Timer
debug = log Debug
notice = log Notice
warn = log Warn
error = log Error

-- Yay permutation game.  I could probably do a typeclass trick to make 'stack'
-- an optional arg, but I think I'd wind up with all the same boilerplate here.
debug_stack, notice_stack, warn_stack, error_stack
    :: (CallStack.Stack, LogMonad m) => Stack.Stack -> Text -> m ()
debug_stack = log_stack Debug
notice_stack = log_stack Notice
warn_stack = log_stack Warn
error_stack = log_stack Error

-- | Write a Debug msg with arbitrary data attached to 'msg_data'.
debug_data :: (CallStack.Stack, LogMonad m, Dynamic.Typeable a) =>
    Text -> a -> m ()
debug_data text data_ = write . add_data =<< make_msg Debug Nothing text
    where add_data msg = msg { msg_data = Data (Dynamic.toDyn data_) }

-- | Prefix msgs with the given string.
add_prefix :: Text -> [Msg] -> [Msg]
add_prefix pref = map $ \m -> m { msg_text = pref <> ": " <> msg_text m }

-- | Write log msgs with 'trace', for debugging.
trace_logs :: [Msg] -> a -> a
trace_logs logs val
    | null logs = val
    | otherwise = Debug.trace_str
        (Text.stripEnd $ Text.unlines $ "\tlogged:" : map format_msg logs)
        val

-- * LogT

class Monad m => LogMonad m where
    write :: Msg -> m ()

    -- | An instance for whatever monad you're using can use this to add some
    -- custom data, like stack information.
    initialize_msg :: Msg -> m Msg
    initialize_msg = return

instance LogMonad IO where
    -- Never write msgs with data, because the data won't survive the
    -- serialization anyway.
    write log_msg = unless (has_data log_msg) $ do
        -- This is also done by 'initialize_msg', but if the msg was created
        -- outside of IO, it won't have had IO's 'initialize_msg' run on it.
        MVar.withMVar global_state $ \(State write_msg prio) ->
            when (prio <= msg_priority log_msg) $
                write_msg =<< add_time log_msg
        when (msg_priority log_msg >= Error) $ do
            log_msg <- add_time log_msg
            Text.IO.hPutStrLn IO.stderr (format_msg log_msg)

    initialize_msg = add_time

-- | Format a msg in a nice user readable way.
format_msg :: Msg -> Text
format_msg (Msg _date caller prio stack text _data) =
    log_msg <> maybe "" ((" "<>) . pretty) stack
    where
    prio_stars Timer = "-"
    prio_stars prio = Text.replicate (fromEnum prio) "*"
    log_msg = mconcat
        [ Text.justifyLeft 5 ' ' (prio_stars prio)
        , CallStack.showCaller caller
        , " - "
        , text
        ]

-- | Add a time to the msg if it doesn't already have one.  Msgs can be logged
-- outside of IO, so they don't get a date until they are written.
add_time :: Msg -> IO Msg
add_time log_msg
    | msg_date log_msg == no_date_yet = do
        utc <- Time.getCurrentTime
        return $! log_msg { msg_date = utc }
    | otherwise = return log_msg

instance Monad m => LogMonad (LogT m) where
    write = write_msg

write_msg :: Monad m => Msg -> LogT m ()
write_msg = LogT . Logger.log

type LogM m = Logger.LoggerT Msg m
newtype LogT m a = LogT { run_log_t :: LogM m a }
    deriving (Applicative.Applicative, Functor, Monad, Trans.MonadIO,
        Trans.MonadTrans, Except.MonadError e, State.MonadState st,
        Reader.MonadReader r)

run :: Monad m => LogT m a -> m (a, [Msg])
run = Logger.run . run_log_t

-- ** mtl instances

instance LogMonad m => LogMonad (State.Strict.StateT s m) where
    write = Trans.lift . write
instance LogMonad m => LogMonad (State.Lazy.StateT s m) where
    write = Trans.lift . write
instance (Error.Error e, LogMonad m) => LogMonad (Error.ErrorT e m) where
    write = Trans.lift . write
instance LogMonad m => LogMonad (Except.ExceptT e m) where
    write = Trans.lift . write
instance LogMonad m => LogMonad (Reader.ReaderT r m) where
    write = Trans.lift . write
instance (Monoid.Monoid w, LogMonad m) => LogMonad (Writer.WriterT w m) where
    write = Trans.lift . write

-- * serialize

-- | Serialize a log msg.  Newline separated text is nice because it's human
-- readable and can use newlines for records.  Previously I used Show, which is
-- bulky and slow.  JSON is hopefully faster, and retains the benefits of Show.
serialize :: Msg -> ByteString.Lazy.ByteString
serialize (Msg date caller prio stack text _data) =
    Aeson.encode $ Aeson.Array $ Vector.fromList
        [toJSON date, toJSON caller, toJSON prio, toJSON stack, toJSON text]

deserialize :: ByteString.Lazy.ByteString -> Either String Msg
deserialize bytes = case Aeson.decode bytes of
    Just (Aeson.Array a) -> case Vector.toList a of
        [date, caller, prio, stack, text] ->
            flip Aeson.Types.parseEither () $ \() ->
                Msg <$> parseJSON date <*> parseJSON caller
                    <*> parseJSON prio <*> parseJSON stack <*> parseJSON text
                    <*> return NoData
        _ -> Left "expected a 5 element array"
    _ -> Left "can't decode json"

instance Aeson.ToJSON Prio
instance Aeson.FromJSON Prio

-- * util

-- | Run an action and report the time in CPU seconds and wall clock seconds.
time_eval :: Trans.MonadIO m => m a -> m (a, Double, Double)
time_eval op = do
    start_cpu <- liftIO CPUTime.getCPUTime
    start <- liftIO Time.getCurrentTime
    !val <- op
    end_cpu <- liftIO CPUTime.getCPUTime
    end <- liftIO Time.getCurrentTime
    let elapsed = end `Time.diffUTCTime` start
    return (val, cpu_to_sec (end_cpu - start_cpu), realToFrac elapsed)
    where
    cpu_to_sec :: Integer -> Double
    cpu_to_sec s = fromIntegral s / 10^12

format_time :: (a, Double, Double) -> (a, Text)
format_time (val, cpu, wall) =
    (val, pretty cpu <> "cpu / " <> pretty wall <> "s")
