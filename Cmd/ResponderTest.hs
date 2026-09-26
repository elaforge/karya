-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | This is similar in intent to "Cmd.CmdTest", but it simulates the entire
-- respond loop.  So it's more accurate and can test more, but is less
-- convenient in that you can't run Cmds directly (you have to use
-- 'respond_cmd').  In addition, you can't run the tests from ghci since it
-- winds up linking in the GUI libs, even though there's no GUI.
--
-- TODO shouldn't it be possible to lift this restriction?
module Cmd.ResponderTest (
    -- * States
    States
    , mkstates, mkstatesBlocks, mkCmdState
    -- * Result
    , Result(..), Seconds, printResults, resultStates, resultUiState
    , resultCmdState
    , resultPerf
    -- * run
    , respondCmd
    , respondAll, continueAll
    , thread, threadDelay
) where
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar

import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text.IO

import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import qualified App.Config as Config
import qualified App.ReplProtocol as ReplProtocol
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance
import qualified Cmd.Repl as Repl
import qualified Cmd.Responder as Responder

import qualified Midi.Interface as Interface
import qualified Midi.StubMidi as StubMidi
import qualified Ui.Fltk as Fltk
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import           Global
import           Types


-- * setup

type States = (Ui.State, Cmd.State)

-- | Make a UI state with one block with the given tracks, and a standard cmd
-- state.
mkstates :: [UiTest.TrackSpec] -> States
mkstates tracks = mkstatesBlocks [(UiTest.defaultBlockName, tracks)]

mkstatesBlocks :: [UiTest.BlockSpec] -> (Ui.State, Cmd.State)
mkstatesBlocks blocks =
    (uiState, mkCmdState uiState UiTest.defaultViewId)
    where
    uiState = UiTest.exec Ui.empty $ do
        rootId : _ <- UiTest.mkblocks blocks
        viewId <- UiTest.mkview rootId
        Ui.setSelection viewId Config.insertSelnum $
            Just $ Sel.Selection
                { startTrack = 1, startPos = 0
                , curTrack = 1, curPos = 0
                , orientation = Sel.Positive
                }

-- | Many cmds rely on a focused view, and it's easy to forget to add it, so
-- make it mandatory.
mkCmdState :: Ui.State -> ViewId -> Cmd.State
mkCmdState uiState viewId = CmdTest.defaultCmdState
    { Cmd.stateFocusedView = Just viewId
    -- Normally this is created by the setup cmd, so pretend I did one.
    , Cmd.stateHistory = Cmd.History [] present [] Nothing
    }
    where present = Cmd.HistoryEntry uiState mempty ["setup"] Nothing


-- * result

data Result = Result
    -- | Msg that gave rise to this Result.
    { resultMsg :: Msg.Msg
    -- | In CmdTest, Result has MIDI from the thru mechanism, and updates
    -- collected during the event.  But responder tests scope over the entire
    -- respond cycle, so MIDI potentially also includes play output and
    -- updates include diff output.
    , resultCmd :: CmdTest.Result Cmd.Status
    -- | These are the updates emitted to the UI.  The CmdTest.Result updates
    -- are those collected during the cmd.
    , resultUpdates :: [Update.DisplayUpdate]
    , resultLoopback :: Chan.Chan Msg.Msg
    -- | CPU seconds it took for this Result to come back.  This doesn't
    -- include waiting for the Msg.
    , resultTime :: Seconds
    , resultLogs :: [Log.Msg]
    }

type Seconds = Double

instance Pretty Result where
    format result = Pretty.format (resultMsg result) <> " -> "
        <> Pretty.format (CmdTest.resultLogs (resultCmd result))

-- | Print Results and anything interesting in them.
printResults :: [Result] -> IO ()
printResults = mapM_ $ \result -> do
    Pretty.pprint (resultMsg result)
    case resultMsg result of
        Msg.DeriveStatus _ (Msg.DeriveComplete perf _) ->
            mapM_ Pretty.pprint (Cmd.perfLogs perf)
        _ -> pure ()
    let logs = CmdTest.resultLogs (resultCmd result)
    unless (null logs) $ do
        putStrLn "\tcmd logs:"
        mapM_ Pretty.pprint logs

resultStates :: Result -> States
resultStates r = (resultUiState r, resultCmdState r)

resultCmdState :: Result -> Cmd.State
resultCmdState = CmdTest.resultCmdState . resultCmd

resultUiState :: Result -> Ui.State
resultUiState = CmdTest.resultUiState . resultCmd

-- | Wait for a DeriveComplete and get the performance from it.
--
-- This is error-prone because if you call this on a Result that didn't
-- regenerate the performance this will hang until 'read_msg' gives up.
resultPerf :: Result -> IO (BlockId, Cmd.Performance)
resultPerf = getPerf . resultLoopback

getPerf :: Chan.Chan Msg.Msg -> IO (BlockId, Cmd.Performance)
getPerf chan = do
    msg <- readMsg 3 chan
    case msg of
        Nothing -> errorIO "get_perf: time out reading chan"
        Just (Msg.DeriveStatus blockId (Msg.DeriveComplete perf _)) ->
            pure (blockId, perf)
        Just _ -> getPerf chan

-- | Continue feeding loopback msgs into the responder until I see
-- DeriveCompletes for all the blocks I expect to derive.
--
-- This is a hack, because I want to make sure all msgs have been processed,
-- but there's no explicit way for the system to say it's done.  But since the
-- only thing chucking things in loopback is background derivation, I can use
-- what I know of how it works to guess when it's done.
--
-- It's dangerous to look for a particular DeriveComplete because the order
-- in which derive threads complete is non-deterministic.
respondAll :: [BlockId] -- ^ subtract from the expected derivation set
    -> Thread.Seconds -> States -> Cmd.CmdT IO a -> IO [Result]
respondAll blocksGone timeout states cmd = do
    putStrLn "---------- new cmd"
    result <- respondCmd states cmd
    (result:) <$> continueAll blocksGone timeout result

-- | Like 'respond_all', continue until all expected derivations are seen.
continueAll :: [BlockId] -- ^ subtract from the expected derivation set
    -> Thread.Seconds -> Result -> IO [Result]
continueAll blocksGone timeout prevResult =
    go Set.empty (resultStates prevResult)
    where
    loopback = resultLoopback prevResult
    expectedBlocks = Performance.deriveBlocks (resultUiState prevResult)
        `Set.difference` Set.fromList blocksGone
    go completeBlocks states
        | completeBlocks == expectedBlocks = pure []
        | otherwise = do
            maybeMsg <- readMsg timeout loopback
            Log.withStdioLock $ Text.IO.putStrLn $
                "ResponderTest.continue: " <> case maybeMsg of
                    Nothing -> "timed out, waiting for "
                        <> pretty expectedBlocks <> ", only got "
                        <> pretty completeBlocks
                    Just msg -> pretty msg
            case maybeMsg of
                Nothing -> pure []
                Just msg -> do
                    result <- respond1 (Just loopback) states Nothing msg
                    let completed = maybe id Set.insert (complete msg)
                            completeBlocks
                    (result:) <$> go completed (resultStates result)
    complete (Msg.DeriveStatus blockId (Msg.DeriveComplete {})) = Just blockId
    complete _ = Nothing

readMsg :: Thread.Seconds -> Chan.Chan Msg.Msg -> IO (Maybe Msg.Msg)
readMsg timeout chan = do
    ci <- maybe False (not . null) <$> Environment.lookupEnv "CI_DONT_TIMEOUT"
    (if ci then fmap Just else Thread.timeout timeout) $ Chan.readChan chan

-- * thread

thread :: Bool -> States -> [Msg.Msg] -> IO [Result]
thread printTiming states msgs =
    threadDelay printTiming states [(m, 0) | m <- msgs]

threadDelay :: Bool -> States -> [(Msg.Msg, Thread.Seconds)] -> IO [Result]
threadDelay _ _ [] = pure []
threadDelay printTiming states ((msg, delay) : msgs) = do
    Printf.printf "thread msg: %s\n" (prettys msg)
    result <- respondMsg states msg
    when printTiming $
        Printf.printf "%s -> lag: %.2fs\n" (prettys msg) (resultTime result)
    Thread.delay delay
    (result:) <$> threadDelay printTiming (resultStates result) msgs

-- * respond_cmd

configureLogging :: IORef.IORef [Log.Msg] -> IO ()
configureLogging logsRef = do
    Log.configure $ \state -> state
        { Log.stateWriteMsg = \msg -> do
            IORef.modifyIORef' logsRef (msg:)
            Log.writeFormatted IO.stdout msg
        -- Otherwise the status updates get spammy.
        , Log.statePriority = Log.Notice
        }
    pure ()

-- | Respond to a single Cmd.  This can be used to test cmds in the full
-- responder context without having to fiddle around with keymaps.
respondCmd :: States -> Cmd.CmdT IO a -> IO Result
respondCmd states cmd = respond1 Nothing states (Just (mkcmd cmd)) magic
    where
    -- I run a cmd by adding a cmd that responds only to a specific Msg, and
    -- then sending that Msg.
    mkcmd cmd msg
        | isMagic msg = cmd >> pure Cmd.Done
        | otherwise = pure Cmd.Continue
    isMagic (Msg.Socket _ (ReplProtocol.QCommand "MAGIC!!")) = True
    isMagic _ = False
    magic = Msg.Socket IO.stdout (ReplProtocol.QCommand "MAGIC!!")

respondMsg :: States -> Msg.Msg -> IO Result
respondMsg states = respond1 Nothing states Nothing

type CmdIO = Msg.Msg -> Cmd.CmdT IO Cmd.Status

respond1 :: Maybe (Chan.Chan Msg.Msg) -> States -> Maybe CmdIO
    -- ^ if given, this Cmd will get the Msg, otherwise whoever would normally
    -- respond will
    -> Msg.Msg -> IO Result
respond1 reuseLoopback (uiState, cmdState) maybeCmd msg = do
    logsRef <- IORef.newIORef []
    configureLogging logsRef
    updateChan <- newChan
    loopbackChan <- maybe Chan.newChan pure reuseLoopback
    (interface, midiChan) <- makeMidiInterface
    uiChan <- MVar.newMVar []
    let rstate = makeRstate uiChan updateChan loopbackChan
            uiState (setCmdState interface) maybeCmd
    ((rstate, midi, updates), metric) <- Thread.timeAction $ do
        (_quit, rstate) <- Responder.respond rstate msg
        midi <- getVals midiChan
        Thread.force midi
        updates <- concat <$> getVals updateChan
        Thread.force updates
        pure (rstate, midi, updates)
    logs <- reverse <$> IORef.readIORef logsRef
    -- Updates and MIDI are normally forced by syncing with the UI and MIDI
    -- driver, so force explicitly here.  Not sure if this really makes
    -- a difference.
    let cmdResult = CmdTest.Result
            { CmdTest.resultVal = Right Nothing
            , CmdTest.resultCmdState = Responder.stateCmd rstate
            , CmdTest.resultUiState = Responder.stateUi rstate
            , CmdTest.resultUiDamage = mempty
            , CmdTest.resultLogs = []
            , CmdTest.resultThru = map Cmd.MidiThru midi
            }
    pure $ Result
        { resultMsg = msg
        , resultCmd = cmdResult
        , resultUpdates = updates
        , resultLoopback = loopbackChan
        , resultTime = realToFrac $ Thread.metricCpu metric
        , resultLogs = logs
        }
    where
    setCmdState interface = cmdState
        { Cmd.stateConfig = (Cmd.stateConfig cmdState)
            { Cmd.configMidiInterface = interface }
        , Cmd.stateDeriveImmediately = Map.keysSet (Ui.stateBlocks uiState)
        }

makeRstate :: Fltk.Channel -> TVar.TVar [[Update.DisplayUpdate]]
    -> Chan.Chan Msg.Msg -> Ui.State -> Cmd.State -> Maybe CmdIO
    -> Responder.State
makeRstate uiChan updateChan loopbackChan uiState cmdState maybeCmd =
    Responder.State
        { Responder.stateStaticConfig = config
        , Responder.stateUi = uiState
        , Responder.stateCmd = cmdState
        -- Tests probably link with the dummy interpreter so this is just
        -- return (), but profiling may try to use the interpreter.
        , Responder.stateSession = Unsafe.unsafePerformIO Repl.makeSession
        , Responder.stateLoopback = loopback
        , Responder.stateSync = dummySync
        , Responder.stateMonitorState = playMonitorState
        , Responder.stateUiChannel = uiChan
        }
    where
    playMonitorState = Unsafe.unsafePerformIO (MVar.newMVar Ui.empty)
    config = StaticConfig.empty
        { StaticConfig.globalCmds = maybe [] (:[]) maybeCmd }
    dummySync _ _ _ updates = do
        putVal updateChan updates
        pure Nothing
    loopback = Chan.writeChan loopbackChan

makeMidiInterface :: IO (Interface.Interface, TVar.TVar [Interface.Message])
makeMidiInterface = do
    midiChan <- newChan
    int <- StubMidi.interface
    let write msg = putVal midiChan msg >> pure Nothing
    pure (int { Interface.writeMessage = write }, midiChan)

-- Use a TVar as a kind of non-blocking channel.
newChan :: IO (TVar.TVar [a])
newChan = TVar.newTVarIO []

putVal :: TVar.TVar [a] -> a -> IO ()
putVal chan v = STM.atomically $ do
    old <- TVar.readTVar chan
    TVar.writeTVar chan (v:old)

getVals :: TVar.TVar [a] -> IO [a]
getVals chan = fmap reverse (STM.readTVarIO chan)
