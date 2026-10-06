-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | The responder is the main event loop on the haskell side.

    It receives msgs (described in Cmd.Msg) multiplexed through a set of
    channels which come from various sources: the UI event loop (in its own
    thread), a socket, the MIDI library, etc.  The Msgs are then dispatched
    through Cmds to treat as they will, stopping when one returns Cmd.Done.

    The responder then deals with the results of the Cmds: midi thru output is
    sent and the old state is diffed with the new state to produce Updates,
    which are given to Sync to sync the visible UI with the changes the Cmds
    made to the UI state.
-}
module Cmd.Responder (
    createMsgReader, responder

#ifdef TESTING
    , respond, State(..)
#endif
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as Monad.State

import qualified Data.Map as Map
import qualified Data.Text.IO as Text.IO
import qualified Network.Socket as Socket
import qualified System.IO as IO

import qualified Util.Debug as Debug
import qualified Util.Log as Log
import qualified Util.Rect as Rect
import qualified Util.Thread as Thread
import qualified Util.Trace as Trace

import qualified App.Config as Config
import qualified App.Path as Path
import qualified App.ReplProtocol as ReplProtocol
import qualified App.StaticConfig as StaticConfig

import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Integrate as Integrate
import qualified Cmd.Internal as Internal
import qualified Cmd.Ky as Ky
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayC as PlayC
import qualified Cmd.Repl as Repl
import qualified Cmd.ResponderSync as ResponderSync
import qualified Cmd.Save as Save
import qualified Cmd.SaveGitT as SaveGitT
import qualified Cmd.SyncKeycaps as SyncKeycaps
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Track as Track
import qualified Cmd.Undo as Undo

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Perform.Sc.Play as Sc.Play
import qualified Perform.Transport as Transport
import qualified Synth.Shared.Thru as Shared.Thru
import qualified Ui.Diff as Diff
import qualified Ui.Fltk as Fltk
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Sync as Sync
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import           Control.Monad
import           Global
import           Types


data State = State
    { stateStaticConfig :: StaticConfig.StaticConfig
    , stateUi :: Ui.State
    , stateCmd :: Cmd.State
    -- | Channel to send IO actions to be executed on the FLTK event loop.
    , stateUiChannel :: Fltk.Channel
    -- | State for the repl subsystem.
    , stateSession :: Repl.Session
    -- | This is used to feed msgs back into the MsgReader.
    , stateLoopback :: Loopback
    -- | This function takes diffs and actually applies them to the UI.  It's
    -- passed as an argument so tests can run the responder without a UI.
    , stateSync :: ResponderSync.Sync

    -- | Communication channel between the play monitor thread and the player,
    -- passed to 'Transport.info_state'.
    , stateMonitorState :: MVar.MVar Ui.State
    }

stateTransportInfo :: State -> Transport.Info
stateTransportInfo state = Transport.Info
    { infoSendStatus = stateLoopback state . Msg.Transport
    , infoMidiWriter = Cmd.stateMidiWriter (stateCmd state)
    , infoMidiAbort = Interface.abort interface
    , infoGetCurrentTime = Interface.now interface
    , infoState = stateMonitorState state
    }
    where
    interface = Cmd.configMidiInterface (Cmd.stateConfig (stateCmd state))

type MsgReader = IO Msg.Msg
type Loopback = Msg.Msg -> IO ()

responder :: StaticConfig.StaticConfig -> SaveGitT.User -> [Rect.Rect]
    -> Fltk.Channel -> MsgReader -> Interface.Interface
    -> Cmd.CmdT IO Cmd.Status -> Repl.Session -> Loopback -> IO ()
responder config gitUser screens uiChan msgReader midiInterface setupCmd
        replSession loopback = do
    Log.debug "start responder"
    uiState <- Ui.create
    monitorState <- MVar.newMVar uiState
    appDir <- Path.getAppDir
    saveDir <- Path.canonical $ Path.toAbsolute appDir Config.saveDir
    let cmdState = setupState screens $ Cmd.initialState $
            StaticConfig.cmdConfig appDir saveDir midiInterface config
                gitUser
    Trace.trace "respond_initialize"
    state <- runSetupCmd setupCmd $ State
        { stateStaticConfig = config
        , stateUi = uiState
        , stateCmd = cmdState
        , stateUiChannel = uiChan
        , stateSession = replSession
        , stateLoopback = loopback
        , stateSync = Sync.sync uiChan
        , stateMonitorState = monitorState
        }
    respondLoop state msgReader

-- | TODO This should probably go in StaticConfig, or better StaticConfig could
-- just directly provide the Cmd.State.  But it needs to take app_dir and
-- interface as args, so... too much work for now.
setupState :: [Rect.Rect] -> Cmd.State -> Cmd.State
setupState screens state = state
    { Cmd.stateEdit = (Cmd.stateEdit state)
        { Cmd.stateTimeStep = TimeStep.timeStep $
            TimeStep.AbsoluteMark TimeStep.AllMarklists Meter.Q
        }
    , Cmd.stateHooks = (Cmd.stateHooks state)
        { Cmd.hooksSelection = Internal.defaultSelectionHooks }
    , Cmd.stateScreens = screens
    }

-- | A special run-and-sync that runs before the respond loop gets started.
runSetupCmd :: Cmd.CmdT IO Cmd.Status -> State -> IO State
runSetupCmd cmd state = fmap snd $ runResponder state $ do
    result <- runContinue True "initial setup" $ Right $ do
        cmd
        Cmd.modify $ \st -> st
            { Cmd.stateHistory = (Cmd.stateHistory st)
                -- If the cmd set hist_last_cmd, don't override it.
                { Cmd.histLastCmd = Cmd.histLastCmd (Cmd.stateHistory st)
                    <|> Just (Cmd.Load Nothing ["setup"])
                }
            }
        pure Cmd.Continue
    whenJust result $ \(_, uiState, cmdState) -> do
        Monad.State.modify $ \st ->
            st { rstateUiTo = uiState, rstateCmdTo = cmdState }
        runSyncStatus
    pure $ Right Cmd.Continue

sendDeriveStatus :: Loopback -> BlockId -> Msg.DeriveStatus -> IO ()
sendDeriveStatus loopback blockId status =
    loopback (Msg.DeriveStatus blockId status)

-- | Create the MsgReader to pass to 'responder'.
createMsgReader ::
    (Midi.ReadMessage -> Midi.ReadMessage) -> TChan.TChan Midi.ReadMessage
    -> Socket.Socket -> TChan.TChan UiMsg.UiMsg -> TChan.TChan Msg.Msg
    -> IO MsgReader
createMsgReader remapRmsg midiChan replSocket uiChan loopbackChan = do
    replChan <- TChan.newTChanIO
    Thread.startLogged "accept repl socket" $ acceptLoop replSocket replChan
    pure $ STM.atomically $
        (Msg.Ui <$> TChan.readTChan uiChan)
        `STM.orElse` (Msg.Midi . remapRmsg <$> TChan.readTChan midiChan)
        `STM.orElse` (uncurry Msg.Socket <$> TChan.readTChan replChan)
        `STM.orElse` (TChan.readTChan loopbackChan)

-- | Accept a connection on the socket, read everything that comes over, then
-- place the socket and the read data on @output_chan@.  It's the caller's
-- responsibility to close the handle after it uses it to reply.
acceptLoop :: Socket.Socket -> TChan.TChan (IO.Handle, ReplProtocol.Query)
    -> IO ()
acceptLoop socket outputChan = forever $ whenJustM (Repl.acceptMsg socket) $
    STM.atomically . TChan.writeTChan outputChan


-- * respond

-- | Kill the respond if it's taking too long.  This can happen if derive gets
-- stuck and you hit play, for instance.
--
-- 2 seconds is too short, initial REPL cmds can take that much time.
-- TODO find out why and fix it?  That might be hard because presumably it's
-- forcing a bunch of CAFs.
timeout :: Thread.Seconds
timeout = 5

respondLoop :: State -> MsgReader -> IO ()
respondLoop state msgReader = do
    Trace.trace "wait"
    msg <- msgReader `Exception.onException` killThreads state
    Trace.trace $ "respond " <> untxt (Msg.showShort msg)
    when (Cmd.stateDebug (stateCmd state)) $
        Debug.putp "msg" msg
    result <- Exception.try $ Thread.timeout timeout $ respond state msg
    case result of
        Left (exc :: Exception.SomeException) -> do
            Log.error $ "exception caught in respond_loop: " <> showt exc
            respondLoop state msgReader
        Right Nothing -> do
            Log.error "respond timed out, derive might be stuck"
            respondLoop state msgReader
        Right (Just (quit, state))
            | quit -> killThreads state
            | otherwise -> respondLoop state msgReader

-- | Kill any active performance threads.  If they are managing subprocesses,
-- this will make sure the subprocesses die too.
killThreads :: State -> IO ()
killThreads = Cmd.killPerformanceThreads. stateCmd

-- | State maintained for a single responder cycle.
data RState = RState
    { rstateState :: !State
    -- | Pre rollback UI state, revert to this state if an exception is
    -- thrown, and diff from this state.
    , rstateUiFrom :: !Ui.State
    -- | Post rollback UI state, and diff to this state.
    , rstateUiTo :: !Ui.State
    , rstateCmdFrom :: !Cmd.State
    , rstateCmdTo :: !Cmd.State
    -- | Collect updates from each cmd.
    , rstateUiDamage :: !Update.UiDamage
    }

makeRstate :: State -> RState
makeRstate state = RState
    { rstateState = state
    , rstateUiFrom = stateUi state
    , rstateUiTo = stateUi state
    , rstateCmdFrom = stateCmd state
    , rstateCmdTo = stateCmd state
    , rstateUiDamage = mempty
    }

type ResponderM = Monad.State.StateT RState IO

newtype Done = Done Result

type Result = Either Ui.Error Cmd.Status

saveDamage :: Update.UiDamage -> ResponderM ()
saveDamage damage = Monad.State.modify $ \st ->
    st { rstateUiDamage = damage <> rstateUiDamage st }

-- ** run

{- | Run one responder cycle.  This is simple in theory: each cmd gets
    a crack at the Msg and the first one to return anything other than
    Continue aborts the sequence.  If a cmd throws an exception the sequence
    is also aborted, and changes to 'Cmd.State' or 'Ui.State' are
    discarded.  Otherwise, the UI state changes are synced with the UI, and
    the responder goes back to waiting for another msg.

    However, there are a number of complications.  Some cmds apply to either
    the cmd state or UI state *before* the rollback, so they don't get rolled
    back on an exception, and don't sync with the UI, or run after the last
    Done, but not after an exception.  Most cmds are pure, but there is
    a hardcoded set that require IO, and many of those need access to
    special state.  Rather than cluttering up Cmd.State for everyone, they
    are passed their special values directly.

    TODO I feel like this generates a lot of garbage per msg.  It mostly
    doesn't matter except for MIDI input.  Profile?
-}
runResponder :: State -> ResponderM Result -> IO (Bool, State)
runResponder state action = do
    (val, RState _ uiFrom uiTo cmdFrom cmdTo uiDamage)
        <- Monad.State.runStateT action (makeRstate state)
    case val of
        Left err -> do
            Log.warn (pretty err)
            -- Exception rolls back changes to ui_state and cmd_state.
            pure (False, state { stateUi = uiFrom, stateCmd = cmdFrom })
        Right status -> do
            -- overload "turned on debug" to dump fltk debugging
            when (not (Cmd.stateDebug cmdFrom) && Cmd.stateDebug cmdTo) $
                forM_ (Map.keys (Ui.stateViews uiTo)) $
                    Sync.printDebug (stateUiChannel state)
            postCmd state uiFrom uiTo cmdTo uiDamage status

-- | Do all the miscellaneous things that need to be done after a command
-- completes.  This doesn't happen if the cmd threw an exception.
postCmd :: State -> Ui.State -> Ui.State -> Cmd.State
    -> Update.UiDamage -> Cmd.Status -> IO (Bool, State)
postCmd state uiFrom uiTo cmdTo uiDamage status = do
    Trace.trace "cmd"
    -- Load external definitions and cache them in Cmd.State, so cmds don't
    -- have a dependency on IO.
    (!uiTo, !cmdTo, logs) <- fromMaybe (uiTo, cmdTo, []) <$>
        Ky.update uiTo cmdTo (Just (Ui.config#UiConfig.ky #$ uiTo))
    mapM_ Log.write logs

    Trace.trace "ky"
    !cmdTo <- handleSpecialStatus (stateUiChannel state) uiTo cmdTo
        (stateTransportInfo state) status
    !cmdTo <- pure $ fixCmdState uiTo cmdTo
    (updates, uiTo) <- ResponderSync.sync (stateSync state)
        uiFrom uiTo cmdTo uiDamage
        (Transport.infoState (stateTransportInfo state))
    Trace.trace "sync"

    cmdTo <- do
        -- Kick off the background derivation threads.
        cmdState <- Performance.updatePerformance
            (sendDeriveStatus (stateLoopback state)) uiTo cmdTo
            (Diff.deriveDiff (stateUi state) uiTo uiDamage updates)
        pure $ cmdState { Cmd.stateDeriveImmediately = mempty }
    Trace.trace "derive_diff"

    cmdTo <- Undo.maintainHistory uiTo cmdTo updates
    Trace.trace "undo"
    cmdTo <- syncKeycaps (stateUiChannel state) cmdTo
    Trace.trace "sync_keycaps"
    when (isQuit status) $
        Save.saveViews cmdTo uiTo
            `Exception.catch` \(exc :: Exception.IOException) ->
                -- Otherwise there's no way to quit!
                Log.error $ "failed to write views while quitting: "
                    <> showt exc
    pure
        ( isQuit status
        , state { stateUi = uiTo, stateCmd = cmdTo }
        )
    where
    isQuit Cmd.Quit = True
    isQuit _ = False
    -- | If the focused view is removed, cmd state should stop pointing to it.
    fixCmdState :: Ui.State -> Cmd.State -> Cmd.State
    fixCmdState uiState cmdState = case Cmd.stateFocusedView cmdState of
        Just focus | focus `Map.notMember` Ui.stateViews uiState ->
            cmdState { Cmd.stateFocusedView = Nothing }
        _ -> cmdState

handleSpecialStatus :: Fltk.Channel -> Ui.State -> Cmd.State
    -> Transport.Info -> Cmd.Status -> IO Cmd.State
handleSpecialStatus uiChan uiState cmdState transportInfo = \case
    Cmd.Play args -> do
        -- Cancel an existing player if there is one.  Play.stop also does a
        -- stop_im, but I don't think that's necessary for im, it only plays
        -- one thing at a time anyway.
        mapM_ Transport.stopPlayer $
            Cmd.statePlayControl (Cmd.statePlay cmdState)
        playCtl <- PlayC.play uiChan uiState transportInfo args
        let play = Cmd.statePlay cmdState
        pure $! cmdState
            { Cmd.statePlay = play
                { Cmd.statePlayControl =
                    playCtl : Cmd.statePlayControl play
                }
            }
    Cmd.FloatingInput action -> do
        Fltk.sendAction uiChan "floating_input" $
            Sync.floatingInput uiState action
        pure $! cmdState
            { Cmd.stateEdit = (Cmd.stateEdit cmdState)
                { Cmd.stateFloatingInput = True }
            }
    _ -> pure cmdState

respond :: State -> Msg.Msg -> IO (Bool, State)
respond state msg = runResponder state $ do
    -- Normal cmds abort as son as one returns a non-Continue.
    result <- fmap unerror $ Except.runExceptT $ do
        handleUnsynced msg
        Trace.trace "handle_unsynced"
        runCoreCmds msg
        Trace.trace "core_cmds"
        pure $ Right Cmd.Done
    case result of
        Right _ -> runSyncStatus
        Left _ -> pure ()
    Trace.trace "sync_status"
    pure result
    where unerror = either (\(Done r) -> r) id

-- * keycaps

syncKeycaps :: Fltk.Channel -> Cmd.State -> IO Cmd.State
syncKeycaps uiChan cmdTo = case Cmd.stateKeycapsUpdate cmdTo of
    Nothing -> pure cmdTo
    Just Cmd.KeycapsClose -> do
        Sync.destroyKeycaps uiChan
        pure $ cmdTo
            { Cmd.stateKeycaps = Nothing
            , Cmd.stateKeycapsUpdate = Nothing
            }
    Just (Cmd.KeycapsUpdate state mbWindow bindings) -> do
        whenJust mbWindow $ \(pos, layout) ->
            Sync.createKeycaps uiChan pos layout
        Sync.updateKeycaps uiChan bindings
        pure $ cmdTo
            { Cmd.stateKeycaps = Just state
            , Cmd.stateKeycapsUpdate = Nothing
            }

-- ** special cmds

-- | Record 'UiMsg.UiUpdate's from the UI.  Like normal cmds it can abort
-- processing by returning not-Continue, but it commits its changes to ui_from
-- and cmd_from in addition to ui_to.  This means these changes won't be rolled
-- back on exception and don't participate in the diff and sync.  This is
-- because UiUpdates are reporting changes that already happened, so they can't
-- be rolled back and sending them back to the UI would be silly.
handleUnsynced :: Msg.Msg -> ErrorResponderM ()
handleUnsynced msg = do
    (result, cmdState) <- lift $ runCmd $ Left $
        Internal.handleUnsynced msg
    case result of
        Left err -> Except.throwError $ Done (Left err)
        Right (status, uiState) -> do
            Monad.State.modify $ \st -> st
                { rstateUiFrom = uiState
                , rstateUiTo = uiState
                , rstateCmdFrom = cmdState
                , rstateCmdTo = cmdState
                }
            when (notContinue status) $
                Except.throwError $ Done (Right status)

-- | This runs after normal cmd processing to update various status displays.
-- It doesn't run after an exception, but *should* run after a Done.
--
-- TODO this could go in 'post_cmd', but then post_cmd would have to be in
-- ResponderM.
runSyncStatus :: ResponderM ()
runSyncStatus = do
    rstate <- Monad.State.get
    result <- runContinue False "sync_status" $ Left $ do
        SyncKeycaps.update
        Internal.syncStatus (rstateUiFrom rstate) (rstateCmdFrom rstate)
            (rstateUiDamage rstate)
    whenJust result $ \(_, uiState, cmdState) -> Monad.State.modify $ \st ->
        st { rstateUiTo = uiState, rstateCmdTo = cmdState }

-- ** core cmds

runCoreCmds :: Msg.Msg -> ErrorResponderM ()
runCoreCmds msg = do
    state <- lift $ Monad.State.gets rstateState
    mapM_ (runThrow . Right . ($msg))
        (StaticConfig.globalCmds (stateStaticConfig state))
    -- Focus commands and the rest of the pure commands come first so text
    -- entry can override io bound commands.
    mapM_ (runThrow . Left . ($msg)) pureCmds
    -- Certain commands require IO.  Rather than make everything IO,
    -- I hardcode them in a special list that gets run in IO.
    let ioCmds = hardcodedIoCmds (stateUiChannel state)
            (stateSession state)
    mapM_ (runThrow . Right . ($msg)) ioCmds

pureCmds :: [Msg.Msg -> Cmd.CmdId Cmd.Status]
pureCmds =
    [ Internal.updateUiState
    , Track.trackCmd
    , Integrate.cmdIntegrate
    , Cmd.call GlobalKeymap.pureKeymap
    ]

-- | These are the only commands that run in IO.
hardcodedIoCmds :: Fltk.Channel -> Repl.Session
    -> [Msg.Msg -> Cmd.CmdT IO Cmd.Status]
hardcodedIoCmds uiChan replSession =
    [ Repl.respond replSession
    , PlayC.cmdPlayMsg uiChan
    , Cmd.call GlobalKeymap.ioKeymap
    ]

-- ** run cmds

type EitherCmd = Either (Cmd.CmdId Cmd.Status) (Cmd.CmdT IO Cmd.Status)
type ErrorResponderM = Except.ExceptT Done ResponderM

-- | Run a cmd and ignore the 'Cmd.Status', but log a complaint if it wasn't
-- Continue.
runContinue :: Bool -- ^ A special hack so I don't get confused when I typo
    -- a score filename.
    -> Text -> EitherCmd
    -> ResponderM (Maybe (Cmd.Status, Ui.State, Cmd.State))
runContinue logToStderr caller cmd = do
    (result, cmdState) <- runCmd cmd
    case result of
        Left err -> do
            liftIO $ Log.error $ caller <> ": " <> pretty err
            when logToStderr $ liftIO $
                Text.IO.hPutStrLn IO.stderr $ caller <> ": " <> pretty err
            pure Nothing
        Right (status, uiState) -> do
            when (notContinue status) $ liftIO $
                Log.error $ caller <> ": expected Continue: " <> showt status
            pure $ Just (status, uiState, cmdState)

-- | Run a Cmd, throwing the 'Cmd.Status' if it wasn't Continue.
runThrow :: EitherCmd -> ErrorResponderM ()
runThrow cmd = do
    (result, cmdState) <- lift $ runCmd cmd
    case result of
        Left err -> Except.throwError $ Done (Left err)
        Right (status, uiState) -> do
            Monad.State.modify $ \st ->
                st { rstateUiTo = uiState, rstateCmdTo = cmdState }
            when (notContinue status) $
                Except.throwError $ Done (Right status)

runCmd :: EitherCmd -> ResponderM
    (Either Ui.Error (Cmd.Status, Ui.State), Cmd.State)
runCmd cmd = do
    rstate <- Monad.State.get
    (cmdState, thru, logs, result) <- liftIO $ case cmd of
        Left cmd ->
            Cmd.runIdIo (rstateUiTo rstate) (rstateCmdTo rstate) cmd
        Right cmd ->
            Cmd.runIo (rstateUiTo rstate) (rstateCmdTo rstate) cmd
    liftIO $ do
        mapM_ Log.write logs
        mapM_ (writeThru (Cmd.stateMidiWriter (rstateCmdTo rstate))) thru
    case result of
        Left err -> pure (Left err, cmdState)
        Right (status, uiState, uiDamage) -> do
            saveDamage uiDamage
            pure (Right (status, uiState), cmdState)

writeThru :: (Interface.Message -> IO ()) -> Cmd.Thru -> IO ()
writeThru midiWriter = \case
    Cmd.MidiThru msg -> midiWriter msg
    Cmd.ImThru msg -> Shared.Thru.send msg
    Cmd.OscThru oscs -> Sc.Play.oscThru oscs

notContinue :: Cmd.Status -> Bool
notContinue Cmd.Continue = False
notContinue _ = True
