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
    create_msg_reader, responder

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
import qualified Cmd.Ruler.Meter as Meter
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Track as Track
import qualified Cmd.Undo as Undo

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Perform.Transport as Transport
import qualified Synth.Shared.Osc as Shared.Osc
import qualified Ui.Diff as Diff
import qualified Ui.Fltk as Fltk
import qualified Ui.Sync as Sync
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import           Control.Monad
import           Global
import           Types


data State = State {
    state_static_config :: StaticConfig.StaticConfig
    , state_ui :: Ui.State
    , state_cmd :: Cmd.State
    -- | Channel to send IO actions to be executed on the FLTK event loop.
    , state_ui_channel :: Fltk.Channel
    -- | State for the repl subsystem.
    , state_session :: Repl.Session
    -- | This is used to feed msgs back into the MsgReader.
    , state_loopback :: Loopback
    -- | This function takes diffs and actually applies them to the UI.  It's
    -- passed as an argument so tests can run the responder without a UI.
    , state_sync :: ResponderSync.Sync

    -- | Communication channel between the play monitor thread and the player,
    -- passed to 'Transport.info_state'.
    , state_monitor_state :: MVar.MVar Ui.State
    }

state_transport_info :: State -> Transport.Info
state_transport_info state = Transport.Info
    { Transport.info_send_status = state_loopback state . Msg.Transport
    , Transport.info_midi_writer =
        Cmd.state_midi_writer (state_cmd state)
    , Transport.info_midi_abort = Interface.abort interface
    , Transport.info_get_current_time = Interface.now interface
    , Transport.info_state = state_monitor_state state
    }
    where
    interface = Cmd.config_midi_interface (Cmd.state_config (state_cmd state))

type MsgReader = IO Msg.Msg
type Loopback = Msg.Msg -> IO ()

responder :: StaticConfig.StaticConfig -> SaveGit.User -> Fltk.Channel
    -> MsgReader -> Interface.Interface -> Cmd.CmdT IO Cmd.Status
    -> Repl.Session -> Loopback -> IO ()
responder config git_user ui_chan msg_reader midi_interface setup_cmd
        repl_session loopback = do
    Log.debug "start responder"
    ui_state <- Ui.create
    monitor_state <- MVar.newMVar ui_state
    app_dir <- Path.get_app_dir
    save_dir <- Path.canonical $ Path.to_absolute app_dir Config.save_dir
    let cmd_state = setup_state $ Cmd.initial_state $
            StaticConfig.cmd_config app_dir save_dir midi_interface config
                git_user
    Trace.trace "respond_initialize"
    state <- run_setup_cmd setup_cmd $ State
        { state_static_config = config
        , state_ui = ui_state
        , state_cmd = cmd_state
        , state_ui_channel = ui_chan
        , state_session = repl_session
        , state_loopback = loopback
        , state_sync = Sync.sync ui_chan
        , state_monitor_state = monitor_state
        }
    respond_loop state msg_reader

-- | TODO This should probably go in StaticConfig, or better StaticConfig could
-- just directly provide the Cmd.State.  But it needs to take app_dir and
-- interface as args, so... too much work for now.
setup_state :: Cmd.State -> Cmd.State
setup_state state = state
    { Cmd.state_edit = (Cmd.state_edit state)
        { Cmd.state_time_step = TimeStep.time_step $
            TimeStep.AbsoluteMark TimeStep.AllMarklists Meter.r_4
        }
    , Cmd.state_hooks = (Cmd.state_hooks state)
        { Cmd.hooks_selection = Internal.default_selection_hooks }
    }

-- | A special run-and-sync that runs before the respond loop gets started.
run_setup_cmd :: Cmd.CmdT IO Cmd.Status -> State -> IO State
run_setup_cmd cmd state = fmap snd $ run_responder state $ do
    result <- run_continue True "initial setup" $ Right $ do
        cmd
        Cmd.modify $ \st -> st
            { Cmd.state_history = (Cmd.state_history st)
                -- If the cmd set hist_last_cmd, don't override it.
                { Cmd.hist_last_cmd = Cmd.hist_last_cmd (Cmd.state_history st)
                    <|> Just (Cmd.Load Nothing ["setup"])
                }
            }
        return Cmd.Continue
    whenJust result $ \(_, ui_state, cmd_state) -> do
        Monad.State.modify $ \st ->
            st { rstate_ui_to = ui_state, rstate_cmd_to = cmd_state }
        run_sync_status
    return $ Right Cmd.Continue

send_derive_status :: Loopback -> BlockId -> Msg.DeriveStatus -> IO ()
send_derive_status loopback block_id status =
    loopback (Msg.DeriveStatus block_id status)

-- | Create the MsgReader to pass to 'responder'.
create_msg_reader ::
    (Midi.ReadMessage -> Midi.ReadMessage) -> TChan.TChan Midi.ReadMessage
    -> Socket.Socket -> TChan.TChan UiMsg.UiMsg -> TChan.TChan Msg.Msg
    -> IO MsgReader
create_msg_reader remap_rmsg midi_chan repl_socket ui_chan loopback_chan = do
    repl_chan <- TChan.newTChanIO
    Thread.startLogged "accept repl socket" $
        accept_loop repl_socket repl_chan
    return $ STM.atomically $
        (Msg.Ui <$> TChan.readTChan ui_chan)
        `STM.orElse` (Msg.Midi . remap_rmsg <$> TChan.readTChan midi_chan)
        `STM.orElse` (uncurry Msg.Socket <$> TChan.readTChan repl_chan)
        `STM.orElse` (TChan.readTChan loopback_chan)

-- | Accept a connection on the socket, read everything that comes over, then
-- place the socket and the read data on @output_chan@.  It's the caller's
-- responsibility to close the handle after it uses it to reply.
accept_loop :: Socket.Socket -> TChan.TChan (IO.Handle, ReplProtocol.Query)
    -> IO ()
accept_loop socket output_chan = forever $ catch_io_errors $ do
    (socket, _peer) <- Socket.accept socket
    hdl <- Socket.socketToHandle socket IO.ReadWriteMode
    msg <- ReplProtocol.server_receive hdl
    STM.atomically $ TChan.writeTChan output_chan (hdl, msg)
    where
    catch_io_errors = Exception.handle $ \(exc :: IOError) ->
        Log.warn $ "caught exception from socket read: " <> showt exc


-- * respond

-- | Kill the respond if it's taking too long.  This can happen if derive gets
-- stuck and you hit play, for instance.
--
-- 2 seconds is too short, initial REPL cmds can take that much time.
-- TODO find out why and fix it?  That might be hard because presumably it's
-- forcing a bunch of CAFs.
timeout :: Thread.Seconds
timeout = 5

respond_loop :: State -> MsgReader -> IO ()
respond_loop state msg_reader = do
    Trace.trace "wait"
    msg <- msg_reader `Exception.onException` kill_threads state
    Trace.trace $ "respond " <> untxt (Msg.show_short msg)
    when (Cmd.state_debug_ui_msgs (state_cmd state)) $
        Debug.putp "msg" msg
    result <- Exception.try $ Thread.timeout timeout $ respond state msg
    case result of
        Left (exc :: Exception.SomeException) -> do
            Log.error $ "exception caught in respond_loop: " <> showt exc
            respond_loop state msg_reader
        Right Nothing -> do
            Log.error "respond timed out, derive might be stuck"
            respond_loop state msg_reader
        Right (Just (quit, state))
            | quit -> kill_threads state
            | otherwise -> respond_loop state msg_reader

-- | Kill any active performance threads.  If they are managing subprocesses,
-- this will make sure the subprocesses die too.
kill_threads :: State -> IO ()
kill_threads = Cmd.kill_performance_threads. state_cmd

-- | State maintained for a single responder cycle.
data RState = RState {
    rstate_state :: !State
    -- | Pre rollback UI state, revert to this state if an exception is
    -- thrown, and diff from this state.
    , rstate_ui_from :: !Ui.State
    -- | Post rollback UI state, and diff to this state.
    , rstate_ui_to :: !Ui.State
    , rstate_cmd_from :: !Cmd.State
    , rstate_cmd_to :: !Cmd.State
    -- | Collect updates from each cmd.
    , rstate_ui_damage :: !Update.UiDamage
    }

make_rstate :: State -> RState
make_rstate state = RState
    { rstate_state = state
    , rstate_ui_from = state_ui state
    , rstate_ui_to = state_ui state
    , rstate_cmd_from = state_cmd state
    , rstate_cmd_to = state_cmd state
    , rstate_ui_damage = mempty
    }

type ResponderM = Monad.State.StateT RState IO

newtype Done = Done Result

type Result = Either Ui.Error Cmd.Status

save_damage :: Update.UiDamage -> ResponderM ()
save_damage damage = Monad.State.modify $ \st ->
    st { rstate_ui_damage = damage <> rstate_ui_damage st }

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
run_responder :: State -> ResponderM Result -> IO (Bool, State)
run_responder state action = do
    (val, RState _ ui_from ui_to cmd_from cmd_to ui_damage)
        <- Monad.State.runStateT action (make_rstate state)
    case val of
        Left err -> do
            Log.warn (pretty err)
            -- Exception rolls back changes to ui_state and cmd_state.
            return (False, state { state_ui = ui_from, state_cmd = cmd_from })
        Right status -> post_cmd state ui_from ui_to cmd_to ui_damage status

-- | Do all the miscellaneous things that need to be done after a command
-- completes.  This doesn't happen if the cmd threw an exception.
post_cmd :: State -> Ui.State -> Ui.State -> Cmd.State
    -> Update.UiDamage -> Cmd.Status -> IO (Bool, State)
post_cmd state ui_from ui_to cmd_to ui_damage status = do
    Trace.trace "cmd"
    -- Load external definitions and cache them in Cmd.State, so cmds don't
    -- have a dependency on IO.
    !cmd_to <- Ky.update_cache ui_to cmd_to
    Trace.trace "ky"
    !cmd_to <- handle_special_status (state_ui_channel state) ui_to cmd_to
        (state_transport_info state) status
    !cmd_to <- return $ fix_cmd_state ui_to cmd_to
    (updates, ui_to, cmd_to) <- ResponderSync.sync (state_sync state)
        ui_from ui_to cmd_to ui_damage
        (Transport.info_state (state_transport_info state))
    Trace.trace "sync"

    cmd_to <- do
        -- Kick off the background derivation threads.
        let damage = Diff.derive_diff (state_ui state) ui_to ui_damage updates
        cmd_state <- Performance.update_performance
            (send_derive_status (state_loopback state)) ui_to cmd_to damage
        return $ cmd_state { Cmd.state_derive_immediately = mempty }
    Trace.trace "derive_diff"

    cmd_to <- Undo.maintain_history ui_to cmd_to updates
    Trace.trace "undo"
    cmd_to <- sync_keymap (state_ui_channel state) cmd_to
    Trace.trace "sync_keymap"
    when (is_quit status) $
        Save.save_views cmd_to ui_to
            `Exception.catch` \(exc :: Exception.IOException) ->
                -- Otherwise there's no way to quit!
                Log.error $ "failed to write views while quitting: "
                    <> showt exc
    return
        ( is_quit status
        , state { state_ui = ui_to, state_cmd = cmd_to }
        )
    where
    is_quit Cmd.Quit = True
    is_quit _ = False
    -- | If the focused view is removed, cmd state should stop pointing to it.
    fix_cmd_state :: Ui.State -> Cmd.State -> Cmd.State
    fix_cmd_state ui_state cmd_state = case Cmd.state_focused_view cmd_state of
        Just focus | focus `Map.notMember` Ui.state_views ui_state ->
            cmd_state { Cmd.state_focused_view = Nothing }
        _ -> cmd_state

handle_special_status :: Fltk.Channel -> Ui.State -> Cmd.State
    -> Transport.Info -> Cmd.Status -> IO Cmd.State
handle_special_status ui_chan ui_state cmd_state transport_info = \case
    Cmd.PlayMidi args -> do
        play_ctl <- PlayC.play ui_chan ui_state transport_info args
        return $! cmd_state
            { Cmd.state_play = (Cmd.state_play cmd_state)
                { Cmd.state_play_control = Just play_ctl }
            }
    Cmd.FloatingInput action -> do
        Fltk.send_action ui_chan "floating_input" $
            Sync.floating_input ui_state action
        return $! cmd_state
            { Cmd.state_edit = (Cmd.state_edit cmd_state)
                { Cmd.state_floating_input = True }
            }
    _ -> return cmd_state

respond :: State -> Msg.Msg -> IO (Bool, State)
respond state msg = run_responder state $ do
    record_keys msg
    Trace.trace "keys"
    -- Normal cmds abort as son as one returns a non-Continue.
    result <- fmap unerror $ Except.runExceptT $ do
        record_ui_updates msg
        Trace.trace "ui_updates"
        run_core_cmds msg
        Trace.trace "core_cmds"
        return $ Right Cmd.Done
    case result of
        Right _ -> run_sync_status
        Left _ -> return ()
    Trace.trace "sync_status"
    return result
    where unerror = either (\(Done r) -> r) id

-- * keymap

sync_keymap :: Fltk.Channel -> Cmd.State -> IO Cmd.State
sync_keymap ui_chan cmd_to = case Cmd.state_keymap cmd_to of
    Nothing -> return cmd_to
    Just (Cmd.KeymapUpdate mb_layout bindings) -> do
        whenJust mb_layout $ \(pos, layout) ->
            Sync.create_keymap ui_chan pos layout
        Sync.update_keymap ui_chan bindings
        return $ cmd_to { Cmd.state_keymap = Nothing }

-- ** special cmds

-- | The record keys cmd commits its changes to cmd_from, so if a later cmd
-- throws the key record won't be rolled back.
record_keys :: Msg.Msg -> ResponderM ()
record_keys msg = do
    result <- run_continue False "record_keys" $ Left $
        Internal.cmd_record_keys msg
    whenJust result $ \(_, _, cmd_state) -> Monad.State.modify $ \st ->
        st { rstate_cmd_from = cmd_state, rstate_cmd_to = cmd_state }

-- | Record 'UiMsg.UiUpdate's from the UI.  Like normal cmds it can abort
-- processing by returning not-Continue, but it commits its changes to ui_from
-- instead of ui_to.  This means these changes don't participate in the diff
-- and sync.  This is because UiUpdates are reporting changes that already
-- happened, so they can't be rolled back and sending them back to the UI
-- would be silly.
record_ui_updates :: Msg.Msg -> ErrorResponderM ()
record_ui_updates msg = do
    (result, cmd_state) <- lift $ run_cmd $ Left $
        Internal.cmd_record_ui_updates msg
    case result of
        Left err -> Except.throwError $ Done (Left err)
        Right (status, ui_state) -> do
            Monad.State.modify $ \st -> st
                { rstate_ui_from = ui_state, rstate_ui_to = ui_state
                , rstate_cmd_to = cmd_state
                }
            when (not_continue status) $
                Except.throwError $ Done (Right status)

-- | This runs after normal cmd processing to update various status displays.
-- It doesn't run after an exception, but *should* run after a Done.
--
-- TODO this could go in 'post_cmd', but then post_cmd would have to be in
-- ResponderM.
run_sync_status :: ResponderM ()
run_sync_status = do
    rstate <- Monad.State.get
    result <- run_continue False "sync_status" $ Left $
        Internal.sync_status (rstate_ui_from rstate) (rstate_cmd_from rstate)
            (rstate_ui_damage rstate)
    whenJust result $ \(_, ui_state, cmd_state) -> Monad.State.modify $ \st ->
        st { rstate_ui_to = ui_state, rstate_cmd_to = cmd_state }

-- ** core cmds

run_core_cmds :: Msg.Msg -> ErrorResponderM ()
run_core_cmds msg = do
    state <- lift $ Monad.State.gets rstate_state
    mapM_ (run_throw . Right . ($msg))
        (StaticConfig.global_cmds (state_static_config state))
    -- Focus commands and the rest of the pure commands come first so text
    -- entry can override io bound commands.
    let pure_cmds = hardcoded_cmds ++ GlobalKeymap.pure_cmds
    mapM_ (run_throw . Left . ($msg)) pure_cmds
    -- Certain commands require IO.  Rather than make everything IO,
    -- I hardcode them in a special list that gets run in IO.
    let io_cmds = hardcoded_io_cmds (state_ui_channel state)
            (state_session state)
    mapM_ (run_throw . Right . ($msg)) io_cmds

-- | These cmds always get the first shot at the Msg.
hardcoded_cmds :: [Msg.Msg -> Cmd.CmdId Cmd.Status]
hardcoded_cmds =
    [ Internal.record_focus
    , Internal.update_ui_state
    , Track.track_cmd
    , Integrate.cmd_integrate
    ]

-- | These are the only commands that run in IO.
hardcoded_io_cmds :: Fltk.Channel -> Repl.Session
    -> [Msg.Msg -> Cmd.CmdT IO Cmd.Status]
hardcoded_io_cmds ui_chan repl_session =
    [ Repl.respond repl_session
    , PlayC.cmd_play_msg ui_chan
    ] ++ GlobalKeymap.io_cmds

-- ** run cmds

type EitherCmd = Either (Cmd.CmdId Cmd.Status) (Cmd.CmdT IO Cmd.Status)
type ErrorResponderM = Except.ExceptT Done ResponderM

-- | Run a cmd and ignore the 'Cmd.Status', but log a complaint if it wasn't
-- Continue.
run_continue :: Bool -- ^ A special hack so I don't get confused when I typo
    -- a score filename.
    -> Text -> EitherCmd
    -> ResponderM (Maybe (Cmd.Status, Ui.State, Cmd.State))
run_continue log_to_stderr caller cmd = do
    (result, cmd_state) <- run_cmd cmd
    case result of
        Left err -> do
            liftIO $ Log.error $ caller <> ": " <> pretty err
            when log_to_stderr $ liftIO $
                Text.IO.hPutStrLn IO.stderr $ caller <> ": " <> pretty err
            return Nothing
        Right (status, ui_state) -> do
            when (not_continue status) $ liftIO $
                Log.error $ caller <> ": expected Continue: " <> showt status
            return $ Just (status, ui_state, cmd_state)

-- | Run a Cmd, throwing the 'Cmd.Status' if it wasn't Continue.
run_throw :: EitherCmd -> ErrorResponderM ()
run_throw cmd = do
    (result, cmd_state) <- lift $ run_cmd cmd
    case result of
        Left err -> Except.throwError $ Done (Left err)
        Right (status, ui_state) -> do
            Monad.State.modify $ \st ->
                st { rstate_ui_to = ui_state, rstate_cmd_to = cmd_state }
            when (not_continue status) $
                Except.throwError $ Done (Right status)

run_cmd :: EitherCmd -> ResponderM
    (Either Ui.Error (Cmd.Status, Ui.State), Cmd.State)
run_cmd cmd = do
    rstate <- Monad.State.get
    (cmd_state, thru, logs, result) <- liftIO $ case cmd of
        Left cmd ->
            Cmd.run_id_io (rstate_ui_to rstate) (rstate_cmd_to rstate) cmd
        Right cmd ->
            Cmd.run_io (rstate_ui_to rstate) (rstate_cmd_to rstate) cmd
    liftIO $ do
        mapM_ Log.write logs
        mapM_ (write_thru (Cmd.state_midi_writer (rstate_cmd_to rstate))) thru
    case result of
        Left err -> return (Left err, cmd_state)
        Right (status, ui_state, damage) -> do
            save_damage damage
            return (Right (status, ui_state), cmd_state)

write_thru :: (Interface.Message -> IO ()) -> Cmd.Thru -> IO ()
write_thru midi_writer = \case
    Cmd.MidiThru msg -> midi_writer msg
    Cmd.ImThru osc -> Shared.Osc.send osc

not_continue :: Cmd.Status -> Bool
not_continue Cmd.Continue = False
not_continue _ = True
