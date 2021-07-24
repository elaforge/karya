-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- Monad.Error
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, BangPatterns #-}
{- | Functions for logging.

    Log msgs are used to report everything from errors and debug msgs to status
    reports.  They are collected in a central place that writes them to a file
    in a machine-readable serialized format.
-}
module Util.Log (
    -- * setup
    configure
    , rotate, rotate_config
    , with_stdio_lock
    -- * msgs
    , Msg(..), msg_string
    -- ** data
    , with_int, with_text, with_dyn
    , lookup_int, lookup_text, lookup_dyn
    -- ** other types
    , Priority(..), State(..)
    , write_json, write_formatted
    , msg, msg_call_stack
    , log
    , timer, debug, notice, warn, error
    , debug_stack, notice_stack, warn_stack, error_stack
    , add_prefix
    , trace_logs
    -- * LogT monad
    , LogMonad(..)
    , LogT, run, LogId, run_id
    , format_msg
    , serialize, deserialize
) where
import           Prelude hiding (error, log)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State.Strict
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Writer.Lazy as Writer

import qualified Data.Aeson as Aeson
import           Data.Aeson (parseJSON, toJSON)
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Char as Char
import qualified Data.Dynamic as Dynamic
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Time as Time
import qualified Data.Vector as Vector

import qualified GHC.Generics as Generics
import qualified GHC.Stack
import qualified Numeric
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Posix as Posix
import qualified System.Process as Process

import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.Read as Read

import qualified Util.CallStack as CallStack
import qualified Util.Debug as Debug
import qualified Util.Exceptions as Exceptions
import qualified Util.Logger as Logger
import qualified Util.Serialize as Serialize
import           Util.Serialize (get, get_tag, put, put_tag)

import qualified Derive.Stack as Stack

import           Global


data Msg = Msg {
    msg_date :: !Time.UTCTime
    , msg_caller :: !CallStack.Caller
    , msg_priority :: !Priority
    -- | Msgs which are logged from the deriver may record the position in the
    -- score the msg was emitted.
    , msg_stack :: !(Maybe Stack.Stack)
    -- | Free form text for humans.
    , msg_text :: !Text
    , msg_data :: !(Map.Map Text Data)
    } deriving (Eq, Show, Read)

instance DeepSeq.NFData Msg where
    rnf msg = DeepSeq.rnf (msg_data msg) `seq` msg `seq` ()

msg_string :: Msg -> String
msg_string = Text.unpack . msg_text

instance Pretty Msg where pretty = format_msg

-- ** data

-- | Attach some semi-structured data to a log msg.  Simple data can then be
-- analyzed without having to parse the text.
data Data = NoData | Int !Int | Text !Text
    -- | Sneak out any domain-specific type, probably for debugging.  Since it
    -- can't be serialized, it will turn into @Text (show dyn)@.
    | Dynamic !Dynamic.Dynamic

instance DeepSeq.NFData Data where
    rnf _ = ()

instance Eq Data where
    a == b = case (a, b) of
        (NoData, NoData) -> True
        (Int x, Int y) -> x == y
        (Text x, Text y) -> x == y
        (Dynamic _, Dynamic _) -> False
        _ -> False

with_int :: Text -> Int -> Msg -> Msg
with_int tag = with_data tag . Int

with_text :: Text -> Text -> Msg -> Msg
with_text tag = with_data tag . Text

with_dyn :: Dynamic.Typeable a => Text -> a -> Msg -> Msg
with_dyn tag = with_data tag . Dynamic . Dynamic.toDyn

with_data :: Text -> Data -> Msg -> Msg
with_data tag val msg = msg { msg_data = Map.insert tag val (msg_data msg) }

lookup_int :: Text -> Msg -> Maybe Int
lookup_int tag msg = case Map.lookup tag (msg_data msg) of
    Just (Int v) -> Just v
    _ -> Nothing

lookup_text :: Text -> Msg -> Maybe Text
lookup_text tag msg = case Map.lookup tag (msg_data msg) of
    Just (Text v) -> Just v
    _ -> Nothing

lookup_dyn :: Dynamic.Typeable a => Text -> Msg -> Maybe a
lookup_dyn tag msg = case Map.lookup tag (msg_data msg) of
    Just (Dynamic dyn) -> Dynamic.fromDynamic dyn
    _ -> Nothing

instance Show Data where
    show NoData = "NoData"
    -- This omits parens for negative numbers but I don't care much.
    show (Int int) = "Int " ++ show int
    show (Text text) = "Text " ++ show text
    show (Dynamic dyn) =
        "Dynamic " ++ show (filter (/='"') (show (Dynamic.dynTypeRep dyn)))

instance Read.Read Data where readPrec = Read.lift read_data

read_data :: ReadP.ReadP Data
read_data = ReadP.choice
    [ ReadP.string "NoData" *> pure NoData
    , Int <$> (ReadP.string "Int " *> p_int)
    , Text . Text.pack <$> (ReadP.string "Text " *> p_text)
    , Text . Text.pack <$> (ReadP.string "Dynamic " *> p_text)
    ]
    where
    -- This will break if there is \" in there.  Surely there's a function
    -- to parse a haskell string?
    p_text = ReadP.between (ReadP.string "\"") (ReadP.string "\"")
        (ReadP.munch (/='"'))
    p_int = ReadP.readS_to_P (Numeric.readSigned Numeric.readDec)

-- ** other types

-- | Pure code can't give a date, but making msg_date Maybe makes it awkward
-- for everyone who processes Msgs, so cheat with this.
no_date_yet :: Time.UTCTime
no_date_yet = Time.UTCTime (Time.ModifiedJulianDay 0) 0

-- | Logging state.  Don't log if a handle is Nothing.
data State = State {
    state_write_msg :: Msg -> IO ()
    , state_priority :: Priority
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
    , state_priority = Notice
    }

{-# NOINLINE global_state #-}
global_state :: MVar.MVar State
global_state = Unsafe.unsafePerformIO (MVar.newMVar initial_state)

-- * setup

rotate :: FilePath -> IO IO.Handle
rotate = rotate_config 4 (4 * mb)
    where mb = 1024^2

-- | Get a file handle for writing log msgs, first rotating logs if necessary.
rotate_config :: Int -> Int -> FilePath -> IO IO.Handle
rotate_config keep max_size log_fn = do
    let rotated_fn n = log_fn ++ "." ++ show n ++ ".gz"
    size <- maybe 0 Posix.fileSize <$> ignore (Posix.getFileStatus log_fn)
    when (size >= fromIntegral max_size) $ do
        forM_ (reverse (zip [1..keep] (drop 1 [1..keep]))) $ \(from, to) ->
            ignore $ Directory.renameFile (rotated_fn from) (rotated_fn to)
        let fn = FilePath.dropExtension (rotated_fn 1)
        putStrLn $ "rotate logs " ++ log_fn ++ " -> " ++ fn
        ignore $ Directory.renameFile log_fn fn
        Process.waitForProcess =<< Process.runProcess "gzip" [fn]
            Nothing Nothing Nothing Nothing Nothing
        return ()
    hdl <- IO.openFile log_fn IO.AppendMode
    -- Logs are per-line, so ensure they go out promptly.
    IO.hSetBuffering hdl IO.LineBuffering
    return hdl
    where ignore = Exceptions.ignoreEnoent

-- | Configure the logging system by modifying its internal state.
configure :: (State -> State) -> IO ()
configure f = MVar.modifyMVar_ global_state (with_log_config . f)

with_log_config :: State -> IO State
with_log_config state = do
    config_str <- fromMaybe "" <$> Environment.lookupEnv "LOG_CONFIG"
    either errorIO return $ parse_log_config config_str state

parse_log_config :: String -> State -> Either Text State
parse_log_config str state
    | str == "" = Right state
    | Just prio <- Map.lookup (lower str) priorities =
        Right $ state { state_priority = prio }
    | otherwise = Left $ "can't parse LOG_CONFIG: " <> Text.pack (show str)
    where
    priorities = Map.fromList $ zip (map (lower . show) ps) ps
        where ps = [minBound .. maxBound] :: [Priority]
    lower = map Char.toLower

-- | Reuse the log lock, presumably to write to stdout or stderr.  It doesn't
-- really belong here, but stdout and stderr are already global, so reusing
-- a lock for them doesn't seem like a big deal.
with_stdio_lock :: IO () -> IO ()
with_stdio_lock action = do
    MVar.withMVar global_state $ \state -> action >> return state
    return ()

data Priority =
    -- | Logs to determine where things are hanging when debugging
    -- a performance problem.  Use "LogView.ShowTimers" to show the time
    -- elapsed between Timer logs.
    Timer
    -- | Users don't look at this during normal use, but can be useful for
    -- debugging.
    | Debug
    -- | Informational msgs that the user might want to see.  Progress messages
    -- in e.g. derivation and play status are included here.
    | Notice
    -- | Something went wrong in e.g. derivation.  The user definitely wants to
    -- see this.
    | Warn
    -- | Unexpected error in the app, which may quit.  This is probably due to
    -- a bug.
    | Error
    deriving (Bounded, Enum, Show, Read, Eq, Ord, Generics.Generic)

-- | Create a msg without initializing it, so it doesn't have to be in
-- LogMonad.
msg :: CallStack.Stack => Priority -> Maybe Stack.Stack -> Text -> Msg
msg = msg_call_stack CallStack.callStack

-- | Like 'msg' but when you already have a CallStack.
msg_call_stack :: GHC.Stack.CallStack -> Priority -> Maybe Stack.Stack -> Text
    -> Msg
msg_call_stack call_stack prio stack text =
    Msg no_date_yet (CallStack.caller call_stack) prio stack text mempty

log :: (CallStack.Stack, LogMonad m) => Priority -> Text -> m ()
log prio text = write $ msg prio Nothing text

log_stack :: (CallStack.Stack, LogMonad m) => Priority -> Stack.Stack -> Text
    -> m ()
log_stack prio stack text = write $ msg prio (Just stack) text

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

-- | Prefix a msg with the given string.
add_prefix :: Text -> Msg -> Msg
add_prefix pref m = m { msg_text = pref <> ": " <> msg_text m }

-- | Write log msgs with 'Debug.trace_str', for debugging.
trace_logs :: [Msg] -> a -> a
trace_logs logs val
    | null logs = val
    | otherwise = Debug.trace_str
        (Text.stripEnd $ Text.unlines $ "\tlogged:" : map format_msg logs)
        val

-- * LogT

-- | Previously there was an initialize_msg method, which could use the
-- LogMonad to fill in fields, e.g. 'add_time'.  Those things can happen in
-- 'write' too, but the msg could be created in a different context from the
-- call to 'write'.  In practice, though, I don't do that very much, and when
-- I did it was usually because I wasn't in a LogMonad at all, so I used the
-- pure 'msg' function.
class Monad m => LogMonad m where
    write :: Msg -> m ()

instance LogMonad IO where
    write log_msg = MVar.withMVar global_state $ \(State write_msg prio) ->
        -- global_state also acts as a lock.
        when (prio <= msg_priority log_msg) $ do
            log_msg <- add_time log_msg
            write_msg log_msg

-- | Format a msg in a nice user readable way.
format_msg :: Msg -> Text
format_msg (Msg _date caller prio stack text _data) =
    log_msg <> maybe "" ((" "<>) . pretty) stack
    where
    prio_symbol Timer = "/"
    prio_symbol prio = Text.replicate (fromEnum prio) "-"
    log_msg = mconcat
        [ Text.justifyLeft 5 ' ' (prio_symbol prio)
        , CallStack.showCaller caller
        , " - "
        , text
        ]

-- | Add a time to the msg if it doesn't already have one.
add_time :: Msg -> IO Msg
add_time log_msg
    | msg_date log_msg == no_date_yet = do
        utc <- Time.getCurrentTime
        return $! log_msg { msg_date = utc }
    | otherwise = return $! log_msg

instance Monad m => LogMonad (LogT m) where
    write = write_msg

write_msg :: Monad m => Msg -> LogT m ()
write_msg = LogT . Logger.log

type LogM m = Logger.LoggerT Msg m
newtype LogT m a = LogT { run_log_t :: LogM m a }
    deriving (Applicative, Functor, Monad, Trans.MonadIO,
        Trans.MonadTrans, Except.MonadError e, State.MonadState st,
        Reader.MonadReader r)

run :: Monad m => LogT m a -> m (a, [Msg])
run = Logger.run . run_log_t

type LogId a = LogT Identity.Identity a

run_id :: LogId a -> (a, [Msg])
run_id = Identity.runIdentity . run

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
serialize (Msg date caller prio stack text data_) =
    Aeson.encode $ Aeson.Array $ Vector.fromList
        [ toJSON date, toJSON caller, toJSON prio, toJSON stack, toJSON text
        , toJSON data_
        ]

deserialize :: ByteString.Lazy.ByteString -> Either String Msg
deserialize bytes = case Aeson.decode bytes of
    Just (Aeson.Array a) -> case Vector.toList a of
        [date, caller, prio, stack, text, data_] ->
            flip Aeson.Types.parseEither () $ \() ->
                Msg <$> parseJSON date <*> parseJSON caller
                    <*> parseJSON prio <*> parseJSON stack <*> parseJSON text
                    <*> parseJSON data_
        _ -> Left "expected a 6 element array"
    _ -> Left "can't decode json"

instance Aeson.ToJSON Priority
instance Aeson.FromJSON Priority

instance Aeson.ToJSON Data where
    toJSON d = case d of
        NoData -> Aeson.Null
        Int v -> Aeson.Number (fromIntegral v)
        Text v -> Aeson.String v
        Dynamic v -> Aeson.String (Text.pack (show v))

instance Aeson.FromJSON Data where
    parseJSON json = case json of
        Aeson.Null -> pure NoData
        Aeson.Number v -> pure $ Int (floor v)
        Aeson.String v -> pure $ Text v
        _ -> fail "expecting null, number, or string"

instance Serialize.Serialize Msg where
    put (Msg a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
    get = Msg <$> get <*> get <*> get <*> get <*> get <*> get

instance Serialize.Serialize Priority where
    put = Serialize.put_enum
    get = Serialize.get_enum

instance Serialize.Serialize Data where
    put NoData = put_tag 0
    put (Int a) = put_tag 1 >> put a
    put (Text a) = put_tag 2 >> put a
    put (Dynamic a) = put (Text (showt a))
    get = get_tag >>= \tag -> case tag of
        0 -> return NoData
        1 -> Int <$> get
        2 -> Text <$> get
        _ -> Serialize.bad_tag "Data" tag
