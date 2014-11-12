-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
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
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.State.Strict as Monad.State

import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Map as Map
import qualified Network
import qualified System.IO as IO
import qualified System.Posix.IO as Posix.IO

import Util.Control
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Integrate as Integrate
import qualified Cmd.Internal as Internal
import qualified Cmd.Meter as Meter
import qualified Cmd.Msg as Msg
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayC as PlayC
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Repl as Repl
import qualified Cmd.ResponderSync as ResponderSync
import qualified Cmd.Save as Save
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Track as Track
import qualified Cmd.Undo as Undo

import qualified Derive.Scale.All as Scale.All
import qualified Perform.Transport as Transport
import qualified App.Config as Config
import qualified App.ReplUtil as ReplUtil
import qualified App.StaticConfig as StaticConfig

import Types


data State = State {
    state_static_config :: StaticConfig.StaticConfig
    , state_ui :: State.State
    , state_cmd :: Cmd.State
    -- | Channel to send IO actions to be executed on the FLTK event loop.
    , state_ui_channel :: Ui.Channel
    -- | State for the repl subsystem.
    , state_session :: Repl.Session
    -- | This is used to feed msgs back into the MsgReader.
    , state_loopback :: Loopback
    -- | This function takes diffs and actually applies them to the UI.  It's
    -- passed as an argument so tests can run the responder without a UI.
    , state_sync :: ResponderSync.Sync

    -- | Communication channel between the play monitor thread and the player,
    -- passed to 'Transport.info_state'.
    , state_monitor_state :: MVar.MVar State.State
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
    interface = Cmd.state_midi_interface (Cmd.state_config (state_cmd state))

type MsgReader = IO Msg.Msg
type Loopback = Msg.Msg -> IO ()

responder :: StaticConfig.StaticConfig -> Ui.Channel -> MsgReader
    -> Interface.Interface -> Cmd.CmdIO -> Repl.Session -> Loopback -> IO ()
responder config ui_chan msg_reader midi_interface setup_cmd repl_session
        loopback = do
    Log.debug "start responder"
    ui_state <- State.create
    monitor_state <- MVar.newMVar ui_state
    app_dir <- Config.get_app_dir
    let cmd_state = setup_state $ Cmd.initial_state $
            cmd_config app_dir midi_interface config
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
-- just directly provide the Cmd.State.  But in needs to take app_dir and
-- interface as args, so... too much work for now.
setup_state :: Cmd.State -> Cmd.State
setup_state state = state
    { Cmd.state_edit = (Cmd.state_edit state)
        { Cmd.state_time_step = TimeStep.time_step $
            TimeStep.AbsoluteMark TimeStep.AllMarklists Meter.r_4
        }
    , Cmd.state_hooks = (Cmd.state_hooks state)
        { Cmd.hooks_selection = Internal.default_selection_hooks
        }
    }

-- | Create a 'Cmd.Config'.  It would be nicer in "Cmd.Cmd", but that would
-- be a circular import with "App.StaticConfig".
cmd_config :: FilePath -> Interface.Interface -> StaticConfig.StaticConfig
    -> Cmd.Config
cmd_config app_dir interface config = Cmd.Config
    { Cmd.state_app_dir = app_dir
    , Cmd.state_midi_interface = interface
    , Cmd.state_definition_paths =
        map (Config.make_path app_dir) Config.definition_paths
    , Cmd.state_rdev_map = StaticConfig.rdev_map midi
    , Cmd.state_wdev_map = StaticConfig.wdev_map midi
    , Cmd.state_instrument_db = StaticConfig.instrument_db config
    , Cmd.state_library = StaticConfig.library config
    -- TODO later this should also be merged with static config
    , Cmd.state_lookup_scale = Cmd.LookupScale $
        \scale_id -> Map.lookup scale_id Scale.All.scales
    , Cmd.state_highlight_colors = StaticConfig.highlight_colors config
    }
    where midi = StaticConfig.midi config

-- | A special run-and-sync that runs before the respond loop gets started.
run_setup_cmd :: Cmd.CmdIO -> State -> IO State
run_setup_cmd cmd state = fmap snd $ run_responder False state $ do
    result <- run_continue "initial setup" $ Right $ do
        cmd
        Cmd.modify $ \st -> st
            { Cmd.state_history = (Cmd.state_history st)
                -- If the cmd set hist_last_cmd, don't override it.
                { Cmd.hist_last_cmd = Cmd.hist_last_cmd (Cmd.state_history st)
                    `mplus` Just (Cmd.Load Nothing ["setup"])
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
    -> Network.Socket -> TChan.TChan UiMsg.UiMsg -> TChan.TChan Msg.Msg
    -> IO MsgReader
create_msg_reader remap_rmsg midi_chan repl_socket ui_chan loopback_chan = do
    repl_chan <- TChan.newTChanIO
    Thread.start_logged "accept repl socket" $
        accept_loop repl_socket repl_chan
    return $ STM.atomically $
        fmap Msg.Ui (TChan.readTChan ui_chan)
        `STM.orElse` fmap (Msg.Midi . remap_rmsg) (TChan.readTChan midi_chan)
        `STM.orElse` fmap (uncurry Msg.Socket) (TChan.readTChan repl_chan)
        `STM.orElse` TChan.readTChan loopback_chan

-- | Accept a connection on the socket, read everything that comes over, then
-- place the socket and the read data on @output_chan@.  It's the caller's
-- responsibility to close the handle after it uses it to reply.
accept_loop :: Network.Socket -> TChan.TChan (IO.Handle, Text) -> IO ()
accept_loop socket output_chan = forever $ catch_io_errors $ do
    (hdl, _host, _port) <- Network.accept socket
    -- Make sure subprocesses don't inherit this.
    fd <- Posix.IO.handleToFd hdl -- Closes hdl as a side-effect.
    Posix.IO.setFdOption fd Posix.IO.CloseOnExec True
    hdl <- Posix.IO.fdToHandle fd
    IO.hSetBuffering hdl IO.NoBuffering
    msg <- ByteString.Char8.hGetLine hdl
    STM.atomically $ TChan.writeTChan output_chan
        (hdl, ReplUtil.decode_request msg)
    where
    catch_io_errors = Exception.handle $ \(exc :: IOError) ->
        Log.warn $ "caught exception from socket read: " <> showt exc


-- * respond

respond_loop :: State -> MsgReader -> IO ()
respond_loop rstate msg_reader = do
    msg <- msg_reader
    -- Debug.putp "msg" msg
    result <- Exception.try $ respond rstate msg
    case result of
        Left (exc :: Exception.SomeException) -> do
            Log.error $ "exception caught in respond_loop: " <> showt exc
            respond_loop rstate msg_reader
        Right (quit, rstate) -> unless quit (respond_loop rstate msg_reader)

-- | State maintained for a single responder cycle.
data RState = RState {
    rstate_state :: !State
    -- | Pre rollback UI state, revert to this state if an exception is
    -- thrown, and diff from this state.
    , rstate_ui_from :: !State.State
    -- | Post rollback UI state, and diff to this state.
    , rstate_ui_to :: !State.State
    , rstate_cmd_from :: !Cmd.State
    , rstate_cmd_to :: !Cmd.State
    -- | Collect updates from each cmd.
    , rstate_updates :: ![Update.CmdUpdate]
    }

make_rstate :: State -> RState
make_rstate state = RState state (state_ui state) (state_ui state)
    (state_cmd state) (state_cmd state) []

type ResponderM = Monad.State.StateT RState IO

newtype Done = Done Result
-- Ick.  This goes away when I can upgrade to transformers-4.
instance Error.Error Done where
    strMsg = error . ("Error Responder.Done instance: "++)

type Result = Either State.Error Cmd.Status

save_updates :: [Update.CmdUpdate] -> ResponderM ()
save_updates updates = Monad.State.modify $ \st ->
    st { rstate_updates = updates ++ rstate_updates st }

-- ** run

{- | Run one responder cycle.  This is simple in theory: each cmd gets
    a crack at the Msg and the first one to return anything other than
    Continue aborts the sequence.  If a cmd throws an exception the sequence
    is also aborted, and changes to 'Cmd.State' or 'State.State' are
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
run_responder :: Bool -- ^ If False, don't start background derivation.  This
    -- is so 'run_setup_cmd' doesn't run a redundant derive, which is
    -- ultimately because it needs to wait for 'load_definitions'.
    -- But 'load_definitions' has to run after 'run_setup_cmd' because the
    -- filename to load is in 'State.State'.
    -> State -> ResponderM Result -> IO (Bool, State)
run_responder run_derive state m = do
    (val, RState _ ui_from ui_to cmd_from cmd_to cmd_updates)
        <- Monad.State.runStateT m (make_rstate state)
    case val of
        Left err -> do
            Log.warn (prettyt err)
            -- Exception rolls back changes to ui_state and cmd_state.
            return (False, state { state_ui = ui_from, state_cmd = cmd_from })
        Right status -> post_cmd run_derive state ui_from ui_to cmd_to
            cmd_updates status

-- | Do all the miscellaneous things that need to be done after a command
-- completes.
post_cmd :: Bool -> State -> State.State -> State.State -> Cmd.State
    -> [Update.CmdUpdate] -> Cmd.Status -> IO (Bool, State)
post_cmd run_derive state ui_from ui_to cmd_to cmd_updates status = do
    cmd_to <- handle_special_status (state_ui_channel state) ui_to cmd_to
        (state_transport_info state) status
    cmd_to <- return $ fix_cmd_state ui_to cmd_to
    (updates, ui_to, cmd_to) <- ResponderSync.sync (state_sync state)
        ui_from ui_to cmd_to cmd_updates
        (Transport.info_state (state_transport_info state))

    cmd_to <- if not run_derive then return cmd_to else do
        -- Kick off the background derivation threads.
        let damage = Diff.derive_diff (state_ui state) ui_to updates
        cmd_state <- Performance.update_performance
            (send_derive_status (state_loopback state)) ui_to cmd_to damage
        return $ cmd_state { Cmd.state_derive_immediately = mempty }

    cmd_to <- Undo.maintain_history ui_to cmd_to updates
    when (is_quit status) $
        Save.save_views cmd_to ui_to
    return (is_quit status,
        state { state_ui = ui_to, state_cmd = cmd_to })
    where
    is_quit Cmd.Quit = True
    is_quit _ = False
    -- | If the focused view is removed, cmd state should stop pointing to it.
    fix_cmd_state :: State.State -> Cmd.State -> Cmd.State
    fix_cmd_state ui_state cmd_state = case Cmd.state_focused_view cmd_state of
        Just focus | focus `Map.notMember` State.state_views ui_state ->
            cmd_state { Cmd.state_focused_view = Nothing }
        _ -> cmd_state

handle_special_status :: Ui.Channel -> State.State -> Cmd.State
    -> Transport.Info -> Cmd.Status -> IO Cmd.State
handle_special_status ui_chan ui_state cmd_state transport_info status =
    case status of
        Cmd.PlayMidi args -> do
            play_ctl <- PlayC.play ui_chan ui_state transport_info args
            return $ cmd_state
                { Cmd.state_play = (Cmd.state_play cmd_state)
                    { Cmd.state_play_control = Just play_ctl }
                }
        Cmd.EditInput edit -> do
            Ui.send_action ui_chan $ Sync.edit_input ui_state edit
            return $! cmd_state
                { Cmd.state_edit = (Cmd.state_edit cmd_state)
                    { Cmd.state_edit_input = True }
                }
        _ -> return cmd_state

respond :: State -> Msg.Msg -> IO (Bool, State)
respond state msg = run_responder True state $ do
    record_keys msg
    load_definitions
    -- Normal cmds abort as son as one returns a non-Continue.
    result <- fmap unerror $ Error.runErrorT $ do
        record_ui_updates msg
        run_core_cmds msg
        return $ Right Cmd.Done
    case result of
        Right _ -> run_sync_status
        _ -> return ()
    return result
    where unerror = either (\(Done r) -> r) id

-- ** special cmds

-- | The record keys cmd commits its changes to cmd_from, so if a later cmd
-- throws the key record won't be rolled back.
record_keys :: Msg.Msg -> ResponderM ()
record_keys msg = do
    result <- run_continue "record_keys" $ Left $
        Internal.cmd_record_keys msg
    whenJust result $ \(_, _, cmd_state) -> Monad.State.modify $ \st ->
        st { rstate_cmd_from = cmd_state, rstate_cmd_to = cmd_state }

-- | Load external definitions and cache them in Cmd.State, so cmds don't
-- have a dependency on IO.
load_definitions :: ResponderM ()
load_definitions = do
    rstate <- Monad.State.get
    cmd_state <- liftIO $ PlayUtil.update_definition_cache
        (rstate_ui_to rstate) (rstate_cmd_to rstate)
    Monad.State.put $ rstate { rstate_cmd_to = cmd_state }

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
        Left err -> Error.throwError $ Done (Left err)
        Right (status, ui_state) -> do
            Monad.State.modify $ \st -> st
                { rstate_ui_from = ui_state, rstate_ui_to = ui_state
                , rstate_cmd_to = cmd_state
                }
            when (not_continue status) $
                Error.throwError $ Done (Right status)

-- | This runs after normal cmd processing to update various status displays.
-- It doesn't run after an exception, but *should* run after a Done.
run_sync_status :: ResponderM ()
run_sync_status = do
    rstate <- Monad.State.get
    result <- run_continue "sync_status" $ Left $
        Internal.sync_status (rstate_ui_from rstate)
            (rstate_cmd_from rstate)
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

    let config = state_static_config state
    -- Certain commands require IO.  Rather than make everything IO,
    -- I hardcode them in a special list that gets run in IO.
    let io_cmds = hardcoded_io_cmds (state_ui_channel state)
            (state_session state) (StaticConfig.local_repl_dirs config)
    mapM_ (run_throw . Right . ($msg)) io_cmds

-- | These cmds always get the first shot at the Msg.
hardcoded_cmds :: [Cmd.Cmd]
hardcoded_cmds =
    [Internal.record_focus, Internal.update_ui_state, Track.track_cmd]

-- | These are the only commands that run in IO.
hardcoded_io_cmds :: Ui.Channel -> Repl.Session -> [FilePath]
    -> [Msg.Msg -> Cmd.CmdIO]
hardcoded_io_cmds ui_chan repl_session repl_dirs =
    [ Repl.repl repl_session repl_dirs
    , Integrate.cmd_integrate
    , PlayC.cmd_play_msg ui_chan
    ] ++ GlobalKeymap.io_cmds

-- ** run cmds

type EitherCmd = Either (Cmd.CmdId Cmd.Status) Cmd.CmdIO
type ErrorResponderM = Error.ErrorT Done ResponderM

-- | Run a cmd and ignore the 'Cmd.Status', but log a complaint if it wasn't
-- Continue.
run_continue :: Text -> EitherCmd
    -> ResponderM (Maybe (Cmd.Status, State.State, Cmd.State))
run_continue caller cmd = do
    (result, cmd_state) <- run_cmd cmd
    case result of
        Left err -> do
            liftIO $ Log.error $ caller <> ": " <> prettyt err
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
        Left err -> Error.throwError $ Done (Left err)
        Right (status, ui_state) -> do
            Monad.State.modify $ \st ->
                st { rstate_ui_to = ui_state, rstate_cmd_to = cmd_state }
            when (not_continue status) $
                Error.throwError $ Done (Right status)

run_cmd :: EitherCmd -> ResponderM
    (Either State.Error (Cmd.Status, State.State), Cmd.State)
run_cmd cmd = do
    rstate <- Monad.State.get
    (cmd_state, midi, logs, result) <- liftIO $ case cmd of
        Left cmd ->
            Cmd.run_id_io (rstate_ui_to rstate) (rstate_cmd_to rstate) cmd
        Right cmd ->
            Cmd.run_io (rstate_ui_to rstate) (rstate_cmd_to rstate) cmd
    liftIO $ do
        mapM_ Log.write logs
        mapM_ (Cmd.state_midi_writer (rstate_cmd_to rstate)) midi
    case result of
        Left err -> return (Left err, cmd_state)
        Right (status, ui_state, updates) -> do
            save_updates updates
            return (Right (status, ui_state), cmd_state)

not_continue :: Cmd.Status -> Bool
not_continue Cmd.Continue = False
not_continue _ = True
