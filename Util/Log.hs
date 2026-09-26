-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- | Functions for logging.

    Log msgs are used to report everything from errors and debug msgs to status
    reports.  They are collected in a central place that writes them to a file
    in a machine-readable serialized format.
-}
module Util.Log (
    -- * setup
    configure
    , rotate, rotateConfig
    , withStdioLock
    -- * msgs
    , Msg(..), msgString
    -- ** data
    , withInt, withText, withDyn
    , lookupInt, lookupText, lookupDyn
    -- ** other types
    , Priority(..), State(..)
    , writeJson, writeFormatted
    , msg, msgCallStack
    , log
    , timer, debug, notice, warn, error
    , debugStack, noticeStack, warnStack, errorStack
    , addPrefix
    -- * LogT monad
    , LogMonad(..)
    , LogT, run, LogId, runId
    , formatMsg
    , serialize, deserialize
) where
import           Prelude hiding (error, log)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.DeepSeq as DeepSeq
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
import           GHC.Stack (HasCallStack)

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
import qualified Util.Exceptions as Exceptions
import qualified Util.Logger as Logger
import qualified Util.Serialize as Serialize
import           Util.Serialize (get, getTag, put, putTag)

import qualified Derive.Stack as Stack

import           Global hiding (errorStack)


data Msg = Msg
    { msgDate :: !Time.UTCTime
    , msgCaller :: !CallStack.Caller
    , msgPriority :: !Priority
    -- | Msgs which are logged from the deriver may record the position in the
    -- score the msg was emitted.
    , msgStack :: !(Maybe Stack.Stack)
    -- | Free form text for humans.
    , msgText :: !Text
    , msgData :: !(Map.Map Text Data)
    } deriving (Eq, Show, Read)

instance DeepSeq.NFData Msg where
    rnf msg = DeepSeq.rnf (msgData msg) `seq` msg `seq` ()

msgString :: Msg -> String
msgString = Text.unpack . msgText

instance Pretty Msg where pretty = formatMsg

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

withInt :: Text -> Int -> Msg -> Msg
withInt tag = withData tag . Int

withText :: Text -> Text -> Msg -> Msg
withText tag = withData tag . Text

withDyn :: Dynamic.Typeable a => Text -> a -> Msg -> Msg
withDyn tag = withData tag . Dynamic . Dynamic.toDyn

withData :: Text -> Data -> Msg -> Msg
withData tag val msg = msg { msgData = Map.insert tag val (msgData msg) }

lookupInt :: Text -> Msg -> Maybe Int
lookupInt tag msg = case Map.lookup tag (msgData msg) of
    Just (Int v) -> Just v
    _ -> Nothing

lookupText :: Text -> Msg -> Maybe Text
lookupText tag msg = case Map.lookup tag (msgData msg) of
    Just (Text v) -> Just v
    _ -> Nothing

lookupDyn :: Dynamic.Typeable a => Text -> Msg -> Maybe a
lookupDyn tag msg = case Map.lookup tag (msgData msg) of
    Just (Dynamic dyn) -> Dynamic.fromDynamic dyn
    _ -> Nothing

instance Show Data where
    show NoData = "NoData"
    -- This omits parens for negative numbers but I don't care much.
    show (Int int) = "Int " ++ show int
    show (Text text) = "Text " ++ show text
    show (Dynamic dyn) =
        "Dynamic " ++ show (filter (/='"') (show (Dynamic.dynTypeRep dyn)))

instance Read.Read Data where readPrec = Read.lift readData

readData :: ReadP.ReadP Data
readData = ReadP.choice
    [ ReadP.string "NoData" *> pure NoData
    , Int <$> (ReadP.string "Int " *> pInt)
    , Text . Text.pack <$> (ReadP.string "Text " *> pText)
    , Text . Text.pack <$> (ReadP.string "Dynamic " *> pText)
    ]
    where
    -- This will break if there is \" in there.  Surely there's a function
    -- to parse a haskell string?
    pText = ReadP.between (ReadP.string "\"") (ReadP.string "\"")
        (ReadP.munch (/='"'))
    pInt = ReadP.readS_to_P (Numeric.readSigned Numeric.readDec)

-- ** other types

-- | Pure code can't give a date, but making msg_date Maybe makes it awkward
-- for everyone who processes Msgs, so cheat with this.
noDateYet :: Time.UTCTime
noDateYet = Time.UTCTime (Time.ModifiedJulianDay 0) 0

-- | Logging state.  Don't log if a handle is Nothing.
data State = State
    { stateWriteMsg :: Msg -> IO ()
    , statePriority :: Priority
    }

-- | Write logs as JSON to the given handle.
writeJson :: IO.Handle -> Msg -> IO ()
writeJson hdl msg = do
    ByteString.Lazy.hPut hdl (serialize msg)
    ByteString.Lazy.hPut hdl "\n"
    when (msgPriority msg == Error) $
        Text.IO.putStrLn (formatMsg msg)

-- | Write logs as human-readable text.
writeFormatted :: IO.Handle -> Msg -> IO ()
writeFormatted hdl = Text.IO.hPutStrLn hdl . formatMsg

initialState :: State
initialState = State
    { stateWriteMsg = writeFormatted IO.stderr
    , statePriority = Notice
    }

{-# NOINLINE globalState #-}
globalState :: MVar.MVar State
globalState = Unsafe.unsafePerformIO (MVar.newMVar initialState)

-- * setup

rotate :: FilePath -> IO IO.Handle
rotate = rotateConfig 4 (4 * mb)
    where mb = 1024^2

-- | Get a file handle for writing log msgs, first rotating logs if necessary.
rotateConfig :: Int -> Int -> FilePath -> IO IO.Handle
rotateConfig keep maxSize logFn = do
    let rotatedFn n = logFn ++ "." ++ show n ++ ".gz"
    size <- maybe 0 Posix.fileSize <$> ignore (Posix.getFileStatus logFn)
    when (size >= fromIntegral maxSize) $ do
        forM_ (reverse (zip [1..keep] (drop 1 [1..keep]))) $ \(from, to) ->
            ignore $ Directory.renameFile (rotatedFn from) (rotatedFn to)
        let fn = FilePath.dropExtension (rotatedFn 1)
        IO.hPutStrLn IO.stderr $ "rotate logs " ++ logFn ++ " -> " ++ fn
        ignore $ Directory.renameFile logFn fn
        Process.waitForProcess =<< Process.runProcess "gzip" [fn]
            Nothing Nothing Nothing Nothing Nothing
        pure ()
    hdl <- IO.openFile logFn IO.AppendMode
    -- Logs are per-line, so ensure they go out promptly.
    IO.hSetBuffering hdl IO.LineBuffering
    pure hdl
    where ignore = Exceptions.ignoreEnoent

-- | Configure the logging system by modifying its internal state.
configure :: (State -> State) -> IO ()
configure f = MVar.modifyMVar_ globalState (withLogConfig . f)

withLogConfig :: State -> IO State
withLogConfig state = do
    configStr <- fromMaybe "" <$> Environment.lookupEnv "LOG_CONFIG"
    either errorIO pure $ parseLogConfig configStr state

parseLogConfig :: String -> State -> Either Text State
parseLogConfig str state
    | str == "" = Right state
    | Just prio <- Map.lookup (lower str) priorities =
        Right $ state { statePriority = prio }
    | otherwise = Left $ "can't parse LOG_CONFIG: " <> Text.pack (show str)
    where
    priorities = Map.fromList $ zip (map (lower . show) ps) ps
        where ps = [minBound .. maxBound] :: [Priority]
    lower = map Char.toLower

-- | Reuse the log lock, presumably to write to stdout or stderr.  It doesn't
-- really belong here, but stdout and stderr are already global, so reusing
-- a lock for them doesn't seem like a big deal.
withStdioLock :: IO () -> IO ()
withStdioLock action = do
    MVar.withMVar globalState $ \state -> action >> pure state
    pure ()

data Priority
    -- | Logs to determine where things are hanging when debugging
    -- a performance problem.  Use "LogView.ShowTimers" to show the time
    -- elapsed between Timer logs.
    = Timer
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
msg :: HasCallStack => Priority -> Maybe Stack.Stack -> Text -> Msg
msg = msgCallStack GHC.Stack.callStack

-- | Like 'msg' but when you already have a CallStack.
msgCallStack :: GHC.Stack.CallStack -> Priority -> Maybe Stack.Stack -> Text
    -> Msg
msgCallStack callStack prio stack text =
    Msg noDateYet (CallStack.caller callStack) prio stack text mempty

log :: (HasCallStack, LogMonad m) => Priority -> Text -> m ()
log prio text = write $ msg prio Nothing text

logStack :: (HasCallStack, LogMonad m) => Priority -> Stack.Stack -> Text
    -> m ()
logStack prio stack text = write $ msg prio (Just stack) text

timer, debug, notice, warn, error :: (HasCallStack, LogMonad m) => Text -> m ()
timer = log Timer
debug = log Debug
notice = log Notice
warn = log Warn
error = log Error

-- Yay permutation game.  I could probably do a typeclass trick to make 'stack'
-- an optional arg, but I think I'd wind up with all the same boilerplate here.
debugStack, noticeStack, warnStack, errorStack
    :: (HasCallStack, LogMonad m) => Stack.Stack -> Text -> m ()
debugStack = logStack Debug
noticeStack = logStack Notice
warnStack = logStack Warn
errorStack = logStack Error

-- | Prefix a msg with the given string.
addPrefix :: Text -> Msg -> Msg
addPrefix pref m = m { msgText = pref <> ": " <> msgText m }

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
    write logMsg = MVar.withMVar globalState $
        \(State { stateWriteMsg, statePriority}) ->
            -- global_state also acts as a lock.
            when (statePriority <= msgPriority logMsg) $ do
                logMsg <- addTime logMsg
                stateWriteMsg logMsg

-- | Format a msg in a nice user readable way.
formatMsg :: Msg -> Text
formatMsg (Msg _date caller prio stack text _data) =
    logMsg <> maybe "" ((" "<>) . pretty) stack
    where
    prioSymbol Timer = "/"
    prioSymbol prio = Text.replicate (fromEnum prio) "-"
    logMsg = mconcat
        [ Text.justifyLeft 5 ' ' (prioSymbol prio)
        , CallStack.showCaller caller
        , " - "
        , text
        ]

-- | Add a time to the msg if it doesn't already have one.
addTime :: Msg -> IO Msg
addTime logMsg
    | msgDate logMsg == noDateYet = do
        utc <- Time.getCurrentTime
        pure $! logMsg { msgDate = utc }
    | otherwise = pure $! logMsg

instance Monad m => LogMonad (LogT m) where
    write = writeMsg

writeMsg :: Monad m => Msg -> LogT m ()
writeMsg = LogT . Logger.log

type LogM m = Logger.LoggerT Msg m
newtype LogT m a = LogT { runLogT :: LogM m a }
    deriving (Applicative, Functor, Monad, Trans.MonadIO,
        Trans.MonadTrans, Except.MonadError e, State.MonadState st,
        Reader.MonadReader r)

run :: Monad m => LogT m a -> m (a, [Msg])
run = Logger.run . runLogT

type LogId a = LogT Identity.Identity a

runId :: LogId a -> (a, [Msg])
runId = Identity.runIdentity . run

-- ** mtl instances

instance LogMonad m => LogMonad (State.Strict.StateT s m) where
    write = Trans.lift . write
instance LogMonad m => LogMonad (State.Lazy.StateT s m) where
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

-- TODO: this is only used by ReplProtocol, maybe it could use JSON?
-- Or just send preformatted log msgs.
instance Serialize.Serialize Msg where
    put (Msg a b c d e f) = put a >> put b >> put c >> put d >> put e >> put f
    get = Msg <$> get <*> get <*> get <*> get <*> get <*> get

instance Serialize.Serialize Priority where
    put = Serialize.putEnumUnsafe
    get = Serialize.getEnumUnsafe

instance Serialize.Serialize Data where
    put NoData = putTag 0
    put (Int a) = putTag 1 >> put a
    put (Text a) = putTag 2 >> put a
    put (Dynamic a) = put (Text (showt a))
    get = getTag >>= \tag -> case tag of
        0 -> pure NoData
        1 -> Int <$> get
        2 -> Text <$> get
        _ -> Serialize.badTag "Data" tag
