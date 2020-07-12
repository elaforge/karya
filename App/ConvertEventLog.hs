-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE NamedFieldPuns #-}
-- | Convert ghc eventlog to json for <chrome://tracing>.
module App.ConvertEventLog (main) where
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Word as Word

import qualified GHC.RTS.Events as Events
import qualified System.Environment as Environment

import qualified Util.CallStack as CallStack
import qualified Util.Lens as Lens
import qualified Util.Seq as Seq

import Global


byHsThread :: Bool
byHsThread = False

main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        ["--print", input] -> Events.printEventsIncremental False input
        ["--user", input] ->
            dumpUser -- . Seq.sort_on Events.evTime
                =<< readEvents input
        [input] -> do
            events <- readEvents input
            print (length events)
            mapM_ print $ events
        [input, output] -> do
            events <- (if byHsThread then replaceCapWithTid else id) <$>
                readEvents input
            let converted = inferDurations $ concatMap convertEvent events
            putStrLn $ "write " <> show (length converted) <> " events"
            write output converted
        _ -> putStrLn "usage"

readEvents :: FilePath -> IO [Events.Event]
readEvents = fmap (Events.events . Events.dat) . readLog

readLog :: FilePath -> IO Events.EventLog
readLog = either (CallStack.errorIO . txt) return
    <=< Events.readEventLogFromFile

-- | Write user events in a format for tools/parse_timing.py.
dumpUser :: [Events.Event] -> IO ()
dumpUser events = forM_ events $ \case
    Events.Event { evTime, Events.evSpec = Events.UserMessage msg } ->
        putStrLn $ unwords [show (nsToSec evTime), msg]
    Events.Event { evTime, Events.evSpec = Events.UserMarker msg } ->
        putStrLn $ unwords [show (nsToSec evTime), msg]
    _ -> return ()

write :: FilePath -> [Event] -> IO ()
write fname events = ByteString.Lazy.writeFile fname $
    ("[\n"<>) $ mconcat $ map ((<>",\n") . Aeson.encode) events
    -- Chrome explicitly supports no trailing ]
    -- I could do Aeson.encode events, but then there are no newlines and it's
    -- annoying to view.

-- | Replace evCap with the thread ID running on that HEC.
replaceCapWithTid :: [Events.Event] -> [Events.Event]
replaceCapWithTid = snd . List.mapAccumL go mempty
    where
    go capToThread_ e = case Events.evCap e of
        Nothing -> (capToThread_, e)
        Just cap -> case IntMap.lookup cap capToThread of
            Nothing -> (capToThread, e)
            Just tid -> (capToThread, e { Events.evCap = Just tid })
            where capToThread = update cap (Events.evSpec e) capToThread_
    update cap e = case e of
        Events.RunThread tid -> IntMap.insert cap (fromIntegral tid)
        _ -> id

-- TODO not integrated yet, but only makes sense if byHsThread
convertMetadata :: Events.EventInfo -> Maybe Detail
convertMetadata spec = case spec of
    Events.ThreadLabel tid label -> Just $ Detail
        { _categories = [cGhc, "thread"]
        , _name = "ThreadLabel"
        , _phase = PMetadata $ ThreadName (txt label)
        , _args = []
        }
    _ -> Nothing

-- * convert

convertEvent :: Events.Event -> [Event]
convertEvent e = do
    capability <- mapMaybe Events.evCap [e]
    detail <- maybe [] (:[]) $ msum $
        map ($ Events.evSpec e) [convertGc, convertUser]
    return $ Event
        { _processId = 1
        , _threadId = capability
        , _timestamp = Events.evTime e `div` 1000
        , _detail = detail
        }

convertUser :: Events.EventInfo -> Maybe Detail
convertUser = \case
    Events.UserMessage str -> Just $ case Text.words msg of
        "respond" : args ->
            async "respond" 1 AsyncBegin [("msg", Text.unwords args)]
        ["wait"] -> async "respond" 1 AsyncEnd []
        -- _ | msg `Set.member` respondCycle -> complete msg
        _ -> detail msg [] PMark
        where msg = txt str
    _ -> Nothing
    where
    -- complete name = detail name [] $ PComplete (Complete Nothing)
    async name id phase args = detail name args $ PAsync $ Async
        { _asyncId = id
        , _asyncPhase = phase
        , _asyncScope = Nothing
        }
    detail name args phase = Detail
        { _categories = ["user"]
        , _name = name
        , _phase = phase
        , _args = args
        }

-- | Add durations for respondCycle events.
inferDurations :: [Event] -> [Event]
inferDurations = map infer . Seq.zip_nexts . Seq.sort_on _timestamp
    where
    infer (e, nexts)
        | inCycle e, Just end <- find nexts = ($e) $
            detail#phase #= PComplete (Complete (Just (end - _timestamp e)))
    infer (e, _) = e
    inCycle e = _name (_detail e) `Set.member` respondCycle
    find = fmap _timestamp
        . List.find (\e -> inCycle e || _name (_detail e) == "respond")
        . takeWhile ((/= "respond") . _name . _detail)

-- These should occur after "respond" and before "wait".
respondCycle :: Set Text
respondCycle = Set.fromList
    [ "keys", "ui_updates", "cmds"
    , "ky", "sync", "derive", "undo"
    ]
    -- These are in the expected order.

convertGc :: Events.EventInfo -> Maybe Detail
convertGc spec = do
    phase <- case spec of
        Events.StartGC -> Just $ PDuration Begin
        Events.EndGC -> Just $ PDuration End
        _ -> Nothing
    return $ Detail
        { _categories = [cGhc, "gc"]
        , _name = "gc"
        , _phase = phase
        , _args = []
        }

{-
    Copied from GHC source:

             GCStart     GCWork      GCIdle      GCEnd
    gc start -----> work -----> idle ------+> done -----> gc end
                     |                     |
                     `-------<-------<-----'

    My on notes:

    RequestParGC -> GCWork -> {GCIdle, GCDone}
        -> GlobalSyncGC -> GCStatsGHC -> EndGC
-}

-- TODO there are too many short-lived threads for this to be useful, since
-- chrome doesn't collapse them vertically.
_convertThread :: Events.EventInfo -> Maybe Detail
_convertThread spec = do
    phase <- case spec of
        Events.CreateThread _tid -> Nothing
        Events.RunThread tid -> flow tid AsyncBegin
        Events.StopThread tid _status -> flow tid AsyncEnd
        Events.ThreadRunnable _tid -> Nothing
        Events.MigrateThread _tid _newCap -> Nothing
        Events.WakeupThread _tid _otherCap -> Nothing
        Events.ThreadLabel _tid _label -> Nothing -- metadata?
            -- TODO associate with this tid and put it in the _name
        _ -> Nothing
    return $ Detail
        { _categories = [cGhc, "thread"]
        , _name = "thread " <> showt (_asyncId phase)
        , _phase = PAsync phase
        , _args = case spec of
            Events.StopThread _tid status -> case status of
                Events.NoStatus -> []
                _ -> [("stop", showt status)]
            _ -> []
        }
    where
    flow id phase = Just $ Async
        { _asyncId = fromIntegral id
        , _asyncPhase = phase
        , _asyncScope = Nothing
        }

cGhc :: Text
cGhc = "ghc"

-- * Event

data Event = Event {
    _processId :: !Int
    , _threadId :: !Int
    , _timestamp :: !Nanosecond
    , _detail :: !Detail
    } deriving (Eq, Show)

detail = Lens.lens _detail (\f r -> r { _detail = f (_detail r) })

data Detail = Detail {
    _categories :: ![Text]
    , _name :: !Text
    , _phase :: !Phase
    , _args :: ![(Text, Text)]
    -- Additional fields:
    -- tts: Optional. The thread clock timestamp of the event. The timestamps
    -- are provided at microsecond granularity.
    -- cname: A fixed color name to associate with the event. If provided,
    -- cname must be one of the names listed in trace-viewer's base color
    -- scheme's reserved color names list
    } deriving (Eq, Show)

phase = Lens.lens _phase (\f r -> r { _phase = f (_phase r) })

type Nanosecond = Word.Word64

nsToSec :: Nanosecond -> Double
nsToSec = (/1_000_000_000) . fromIntegral

instance Aeson.ToJSON Event where
    toEncoding (Event pid tid ts (Detail cats name phase_ args)) =
        Aeson.pairs $ mconcat $
            [ "cat" .= Text.intercalate "," cats
            , "name" .= name
            , "args" .= Aeson.object (map (second Aeson.toJSON) args)
            , "pid" .= pid
            , "tid" .= tid
            , "ts" .= ts
            , "ph" .= phase
            ] ++ [k .= v | (k, v) <- fields]
        where
        (phase, fields) = convert phase_
        -- TODO ensure later fields override earlier ones

    -- TODO how to remove the duplication?
    toJSON (Event pid tid ts (Detail cats name _phase args)) = Aeson.object
        [ "cat" .= Text.intercalate "," cats
        , "name" .= name
        , "args" .= args
        , "pid" .= pid
        , "tid" .= tid
        , "ts" .= ts
        -- , "ph" .= phase
        ]

class Convert a where
    -- (value for ph field, extra fields for the top level)
    convert :: a -> (Aeson.Value, [(Text, Aeson.Value)])

data Phase = PDuration !Duration | PComplete !Complete | PInstant !Instant
    | PCounter !Counter | PAsync !Async | PFlow !Flow | PMetadata !Metadata
    -- | PMemoryDump !MemoryDump
    -- | Mark events are created whenever a corresponding navigation timing API
    -- mark is created.
    | PMark
    deriving (Eq, Show)

instance Convert Phase where
    convert = \case
        PDuration duration -> convert duration
        PComplete complete -> convert complete
        PInstant instant -> convert instant
        PCounter counter -> convert counter
        PAsync async -> convert async
        PFlow flow -> convert flow
        PMetadata metadata -> convert metadata
        -- PMemoryDump memoryDump -> convert memoryDump
        PMark -> ("R", [])

-- | These must be strictly nested.
--
-- For Begin events, either stack=[TextFrame], or sf=Int, index into
-- stackFrames map in metadata.
data Duration = Begin | End deriving (Eq, Show)

instance Convert Duration where
    convert = \case
        Begin -> ("B", [])
        End -> ("E", [])

-- tdur=thread clock duration.  Also could have stack or sf.
data Complete = Complete !(Maybe Nanosecond) deriving (Eq, Show)

instance Convert Complete where
    convert (Complete dur) =
        ("X", maybe [] ((:[]) . ("dur",) . Aeson.toJSON) dur)

data Instant = Instant !InstantScope deriving (Eq, Show)
data InstantScope = Global | Process | Thread deriving (Eq, Show)

instance Convert Instant where
    convert (Instant scope) = ("i",) . (:[]) . ("s",) $ case scope of
        Global -> "g"
        Process -> "p"
        Thread -> "t"

data Counter = Counter !(Map Text Int)
    deriving (Eq, Show)

instance Convert Counter where
    convert (Counter args) = ("C", [("args", Aeson.toJSON args)])

-- We consider the events with the same category and id as events from the same
-- event tree.
data Async = Async {
    _asyncId :: !Int
    , _asyncPhase :: !AsyncPhase
    -- | Differentiate events with the same id.
    , _asyncScope :: !(Maybe Text)
    } deriving (Eq, Show)
data AsyncPhase = AsyncBegin | AsyncInstant | AsyncEnd
    deriving (Eq, Show)

instance Convert Async where
    convert (Async id phase scope) =
        (ph, add "scope" scope [("id", Aeson.toJSON id)])
        where
        ph = case phase of
            AsyncBegin -> "b"
            AsyncInstant -> "n"
            AsyncEnd -> "e"

-- | These only show up if they have enclosing Durations.
--
-- Official doc: The flow events are very similar in concept to the Async
-- events, but allow duration to be associated with each other across
-- threads/processes.  Visually, think of a flow event as an arrow between two
-- duration events.  With flow events, each event will be drawn in the thread
-- it is emitted from.  The events will be linked together in Trace Viewer
-- using lines and arrows.
data Flow = Flow {
    _flowId :: !Int
    , _flowPhase :: !AsyncPhase
    -- | Differentiate events with the same id.
    , _flowScope :: !(Maybe Text)
    } deriving (Eq, Show)

instance Convert Flow where
    convert (Flow id phase scope) =
        (ph, add "scope" scope [("id", Aeson.toJSON id)])
        where
        ph = case phase of
            AsyncBegin -> "s"
            AsyncInstant -> "t"
            AsyncEnd -> "f"

-- There are currently 5 possible metadata items that can be provided:
data Metadata =
    -- | Sets the display name for the provided pid. The name is provided in
    -- a name argument.
    ProcessName !Text
    -- | Sets the extra process labels for the provided pid. The label is
    -- provided in a labels argument.
    | ProcessLabels !Text
    -- | Sets the process sort order position. The sort index is provided in
    -- a sort_index argument.
    | ProcessSortIndex !Int
    -- | Sets the name for the given tid. The name is provided in a name
    -- argument.
    | ThreadName !Text
    -- Sets the thread sort order position. The sort index is provided in
    -- a sort_index argument.
    | ThreadSortIndex !Int
    deriving (Eq, Show)

instance Convert Metadata where
    convert m = ("M",) $ case m of
        ProcessName name ->
            [ ("name", "thread_name")
            , ("args", Aeson.object [("name", Aeson.toJSON name)])
            ]
        ThreadName name ->
            [ ("name", "thread_name")
            , ("args", Aeson.object [("name", Aeson.toJSON name)])
            ]
        ProcessLabels label ->
            [ ("name", "process_labels")
            , ("args", Aeson.object [("labels", Aeson.toJSON label)])
            ]
        ProcessSortIndex index ->
            [ ("name", "process_sort_index")
            , ("args", Aeson.object [("sort_index", Aeson.toJSON index)])
            ]
        ThreadSortIndex index ->
            [ ("name", "thread_sort_index")
            , ("args", Aeson.object [("sort_index", Aeson.toJSON index)])
            ]

-- | Memory dump events correspond to memory dumps of (groups of) processes.
-- There are two types of memory dump events:
--
-- - Global memory dump events, which contain system memory information such as
-- the size of RAM, are denoted by the V phase type and
--
-- - Process memory dump events, which contain information about a single
-- process’s memory usage (e.g. total allocated memory), are denoted by the
-- v phase type.
data MemoryDump = MemoryDump {
    _mScope :: !MemoryScope
    , _mDump :: () -- TODO, maybe I can use GCStatsGHC?
    } deriving (Eq, Show)
data MemoryScope = MemoryGlobal | MemoryProcess deriving (Eq, Show)

-- * util

add :: Aeson.ToJSON a => k -> Maybe a -> [(k, Aeson.Value)]
    -> [(k, Aeson.Value)]
add k mbV xs = case mbV of
    Nothing -> xs
    Just v -> (k, Aeson.toJSON v) : xs

{-
data EventInfo
    -- pseudo events
    = EventBlock         { end_time   :: Timestamp,
                           cap        :: Int,
                           block_size :: BlockSize
                         }
    | UnknownEvent       { ref  :: {-# UNPACK #-}!EventTypeNum }

    -- init and shutdown
    | Startup            { n_caps :: Int
                         }
    -- EVENT_SHUTDOWN is replaced by EVENT_CAP_DELETE and GHC 7.6+
    -- no longer generate the event; should be removed at some point
    | Shutdown           { }

    -- thread scheduling
    | CreateThread       { thread :: {-# UNPACK #-}!ThreadId
                         }
    | RunThread          { thread :: {-# UNPACK #-}!ThreadId
                         }
    | StopThread         { thread :: {-# UNPACK #-}!ThreadId,
                           status :: !ThreadStopStatus
                         }
    | ThreadRunnable     { thread :: {-# UNPACK #-}!ThreadId
                         }
    | MigrateThread      { thread :: {-# UNPACK #-}!ThreadId,
                           newCap :: {-# UNPACK #-}!Int
                         }
    | WakeupThread       { thread :: {-# UNPACK #-}!ThreadId,
                           otherCap :: {-# UNPACK #-}!Int
                         }
    | ThreadLabel        { thread :: {-# UNPACK #-}!ThreadId,
                           threadlabel :: String
                         }

    -- par sparks
    | CreateSparkThread  { sparkThread :: {-# UNPACK #-}!ThreadId
                         }
    | SparkCounters      { sparksCreated, sparksDud, sparksOverflowed,
                           sparksConverted, sparksFizzled, sparksGCd,
                           sparksRemaining :: {-# UNPACK #-}! Word64
                         }
    | SparkCreate        { }
    | SparkDud           { }
    | SparkOverflow      { }
    | SparkRun           { }
    | SparkSteal         { victimCap :: {-# UNPACK #-}!Int }
    | SparkFizzle        { }
    | SparkGC            { }

    -- tasks
    | TaskCreate         { taskId :: TaskId,
                           cap :: {-# UNPACK #-}!Int,
                           tid :: {-# UNPACK #-}!KernelThreadId
                         }
    | TaskMigrate        { taskId :: TaskId,
                           cap :: {-# UNPACK #-}!Int,
                           new_cap :: {-# UNPACK #-}!Int
                         }
    | TaskDelete         { taskId :: TaskId }

    -- garbage collection
    | RequestSeqGC       { }
    | RequestParGC       { }
    | StartGC            { }
    | GCWork             { }
    | GCIdle             { }
    | GCDone             { }
    | EndGC              { }
    | GlobalSyncGC       { }
    | GCStatsGHC         { heapCapset   :: {-# UNPACK #-}!Capset
                         , gen          :: {-# UNPACK #-}!Int
                         , copied       :: {-# UNPACK #-}!Word64
                         , slop, frag   :: {-# UNPACK #-}!Word64
                         , parNThreads  :: {-# UNPACK #-}!Int
                         , parMaxCopied :: {-# UNPACK #-}!Word64
                         , parTotCopied :: {-# UNPACK #-}!Word64
                         }

    -- heap statistics
    | HeapAllocated      { heapCapset  :: {-# UNPACK #-}!Capset
                         , allocBytes  :: {-# UNPACK #-}!Word64
                         }
    | HeapSize           { heapCapset  :: {-# UNPACK #-}!Capset
                         , sizeBytes   :: {-# UNPACK #-}!Word64
                         }
    | HeapLive           { heapCapset  :: {-# UNPACK #-}!Capset
                         , liveBytes   :: {-# UNPACK #-}!Word64
                         }
    | HeapInfoGHC        { heapCapset    :: {-# UNPACK #-}!Capset
                         , gens          :: {-# UNPACK #-}!Int
                         , maxHeapSize   :: {-# UNPACK #-}!Word64
                         , allocAreaSize :: {-# UNPACK #-}!Word64
                         , mblockSize    :: {-# UNPACK #-}!Word64
                         , blockSize     :: {-# UNPACK #-}!Word64
                         }

    -- adjusting the number of capabilities on the fly
    | CapCreate          { cap :: {-# UNPACK #-}!Int
                         }
    | CapDelete          { cap :: {-# UNPACK #-}!Int
                         }
    | CapDisable         { cap :: {-# UNPACK #-}!Int
                         }
    | CapEnable          { cap :: {-# UNPACK #-}!Int
                         }

    -- capability sets
    | CapsetCreate       { capset     :: {-# UNPACK #-}!Capset
                         , capsetType :: CapsetType
                         }
    | CapsetDelete       { capset :: {-# UNPACK #-}!Capset
                         }
    | CapsetAssignCap    { capset :: {-# UNPACK #-}!Capset
                         , cap    :: {-# UNPACK #-}!Int
                         }
    | CapsetRemoveCap    { capset :: {-# UNPACK #-}!Capset
                         , cap    :: {-# UNPACK #-}!Int
                         }

    -- program/process info
    | RtsIdentifier      { capset :: {-# UNPACK #-}!Capset
                         , rtsident :: String
                         }
    | ProgramArgs        { capset :: {-# UNPACK #-}!Capset
                         , args   :: [String]
                         }
    | ProgramEnv         { capset :: {-# UNPACK #-}!Capset
                         , env    :: [String]
                         }
    | OsProcessPid       { capset :: {-# UNPACK #-}!Capset
                         , pid    :: {-# UNPACK #-}!PID
                         }
    | OsProcessParentPid { capset :: {-# UNPACK #-}!Capset
                         , ppid   :: {-# UNPACK #-}!PID
                         }
    | WallClockTime      { capset :: {-# UNPACK #-}!Capset
                         , sec    :: {-# UNPACK #-}!Word64
                         , nsec   :: {-# UNPACK #-}!Word32
                         }

    -- messages
    | Message            { msg :: String }
    | UserMessage        { msg :: String }
    | UserMarker         { markername :: String }

    -- Events emitted by a parallel RTS
     -- Program /process info (tools might prefer newer variants above)
    | Version            { version :: String }
    | ProgramInvocation  { commandline :: String }
     -- startup and shutdown (incl. real start time, not first log entry)
    | CreateMachine      { machine :: {-# UNPACK #-} !MachineId,
                           realtime    :: {-# UNPACK #-} !Timestamp}
    | KillMachine        { machine ::  {-# UNPACK #-} !MachineId }
     -- Haskell processes mgmt (thread groups that share heap and communicate)
    | CreateProcess      { process :: {-# UNPACK #-} !ProcessId }
    | KillProcess        { process :: {-# UNPACK #-} !ProcessId }
    | AssignThreadToProcess { thread :: {-# UNPACK #-} !ThreadId,
                              process :: {-# UNPACK #-} !ProcessId
                            }
     -- communication between processes
    | EdenStartReceive   { }
    | EdenEndReceive     { }
    | SendMessage        { mesTag :: !MessageTag,
                           senderProcess :: {-# UNPACK #-} !ProcessId,
                           senderThread :: {-# UNPACK #-} !ThreadId,
                           receiverMachine ::  {-# UNPACK #-} !MachineId,
                           receiverProcess :: {-# UNPACK #-} !ProcessId,
                           receiverInport :: {-# UNPACK #-} !PortId
                         }
    | ReceiveMessage     { mesTag :: !MessageTag,
                           receiverProcess :: {-# UNPACK #-} !ProcessId,
                           receiverInport :: {-# UNPACK #-} !PortId,
                           senderMachine ::  {-# UNPACK #-} !MachineId,
                           senderProcess :: {-# UNPACK #-} !ProcessId,
                           senderThread :: {-# UNPACK #-} !ThreadId,
                           messageSize :: {-# UNPACK #-} !MessageSize
                         }
    | SendReceiveLocalMessage { mesTag :: !MessageTag,
                                senderProcess :: {-# UNPACK #-} !ProcessId,
                                senderThread :: {-# UNPACK #-} !ThreadId,
                                receiverProcess :: {-# UNPACK #-} !ProcessId,
                                receiverInport :: {-# UNPACK #-} !PortId
                              }

    -- These events have been added for Mercury's benifit but are generally
    -- useful.
    | InternString       { str :: String, sId :: {-# UNPACK #-}!StringId }

    -- Mercury specific events.
    | MerStartParConjunction {
          dyn_id      :: {-# UNPACK #-}!ParConjDynId,
          static_id   :: {-# UNPACK #-}!ParConjStaticId
      }
    | MerEndParConjunction {
          dyn_id      :: {-# UNPACK #-}!ParConjDynId
      }
    | MerEndParConjunct {
          dyn_id      :: {-# UNPACK #-}!ParConjDynId
      }
    | MerCreateSpark {
          dyn_id      :: {-# UNPACK #-}!ParConjDynId,
          spark_id    :: {-# UNPACK #-}!SparkId
      }
    | MerFutureCreate {
          future_id   :: {-# UNPACK #-}!FutureId,
          name_id     :: {-# UNPACK #-}!StringId
      }
    | MerFutureWaitNosuspend {
          future_id   :: {-# UNPACK #-}!FutureId
      }
    | MerFutureWaitSuspended {
          future_id   :: {-# UNPACK #-}!FutureId
      }
    | MerFutureSignal {
          future_id   :: {-# UNPACK #-}!FutureId
      }
    | MerLookingForGlobalThread
    | MerWorkStealing
    | MerLookingForLocalSpark
    | MerReleaseThread {
          thread_id   :: {-# UNPACK #-}!ThreadId
      }
    | MerCapSleeping
    | MerCallingMain

    -- perf events
    | PerfName           { perfNum :: {-# UNPACK #-}!PerfEventTypeNum
                         , name    :: String
                         }
    | PerfCounter        { perfNum :: {-# UNPACK #-}!PerfEventTypeNum
                         , tid     :: {-# UNPACK #-}!KernelThreadId
                         , period  :: {-# UNPACK #-}!Word64
                         }
    | PerfTracepoint     { perfNum :: {-# UNPACK #-}!PerfEventTypeNum
                         , tid     :: {-# UNPACK #-}!KernelThreadId
                         }
    | HeapProfBegin      { heapProfId :: !Word8
                         , heapProfSamplingPeriod :: !Word64
                         , heapProfBreakdown :: !HeapProfBreakdown
                         , heapProfModuleFilter :: !Text
                         , heapProfClosureDescrFilter :: !Text
                         , heapProfTypeDescrFilter :: !Text
                         , heapProfCostCentreFilter :: !Text
                         , heapProfCostCentreStackFilter :: !Text
                         , heapProfRetainerFilter :: !Text
                         , heapProfBiographyFilter :: !Text
                         }
    | HeapProfCostCentre { heapProfCostCentreId :: !Word32
                         , heapProfLabel :: !Text
                         , heapProfModule :: !Text
                         , heapProfSrcLoc :: !Text
                         , heapProfFlags :: !HeapProfFlags
                         }
    | HeapProfSampleBegin
                         { heapProfSampleEra :: !Word64
                         }
    | HeapProfSampleCostCentre
                         { heapProfId :: !Word8
                         , heapProfResidency :: !Word64
                         , heapProfStackDepth :: !Word8
                         , heapProfStack :: !(VU.Vector Word32)
                         }
    | HeapProfSampleString
                         { heapProfId :: !Word8
                         , heapProfResidency :: !Word64
                         , heapProfLabel :: !Text
                         }

data ThreadStopStatus
    = NoStatus
    | HeapOverflow
    | StackOverflow
    | ThreadYielding
    | ThreadBlocked
    | ThreadFinished
    | ForeignCall
    | BlockedOnMVar
    | BlockedOnMVarRead   -- since GHC-7.8, see [Stop status since GHC-7.7]
    | BlockedOnBlackHole
    | BlockedOnRead
    | BlockedOnWrite
    | BlockedOnDelay
    | BlockedOnSTM
    | BlockedOnDoProc
    | BlockedOnCCall
    | BlockedOnCCall_NoUnblockExc
    | BlockedOnMsgThrowTo
    | ThreadMigrating
    | BlockedOnMsgGlobalise
    | BlockedOnBlackHoleOwnedBy {-# UNPACK #-}!ThreadId
-}
