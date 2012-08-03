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
import qualified Control.Monad.Trans as Trans

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Network
import qualified System.IO as IO

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Integrate as Integrate
import qualified Cmd.Internal as Internal
import qualified Cmd.Lang as Lang
import qualified Cmd.Lilypond as Lilypond
import qualified Cmd.Msg as Msg
import qualified Cmd.PlayC as PlayC
import qualified Cmd.ResponderSync as ResponderSync
import qualified Cmd.Track as Track
import qualified Cmd.Undo as Undo

import qualified Perform.Transport as Transport
import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig
import Types


data State = State {
    state_static_config :: StaticConfig.StaticConfig
    , state_ui :: State.State
    , state_cmd :: Cmd.State
    -- | State for the lang subsystem.
    , state_session :: Lang.Session
    -- | This is used to feed msgs back into the MsgReader.
    , state_loopback :: Loopback
    -- | This function takes diffs and actually applies them to the UI.  It's
    -- passed as an argument so tests can run the responder without a UI.
    , state_sync :: ResponderSync.Sync

    -- | Communication channel between the updater thread and the player,
    -- passed to 'Transport.info_state'.
    , state_updater_state :: MVar.MVar State.State
    }

state_transport_info :: State -> Transport.Info
state_transport_info state = Transport.Info
    { Transport.info_send_status = state_loopback state . Msg.Transport
    , Transport.info_midi_writer =
        Cmd.state_midi_writer (state_cmd state)
    , Transport.info_midi_abort = Interface.abort interface
    , Transport.info_get_current_time = Interface.now interface
    , Transport.info_state = state_updater_state state
    }
    where interface = Cmd.state_midi_interface (state_cmd state)

type MsgReader = IO Msg.Msg
type Loopback = Msg.Msg -> IO ()

responder :: StaticConfig.StaticConfig -> MsgReader -> Interface.Interface
    -> Cmd.CmdIO -> Lang.Session -> Loopback -> IO ()
responder config msg_reader midi_interface setup_cmd lang_session
        loopback = do
    Log.debug "start responder"

    let cmd_state = Cmd.initial_state
            (StaticConfig.rdev_map config) (StaticConfig.wdev_map config)
            midi_interface
            (StaticConfig.instrument_db config)
            (StaticConfig.global_scope config)
    updater_state <- MVar.newMVar State.empty
    state <- run_setup_cmd setup_cmd $
        State config State.empty cmd_state lang_session loopback Sync.sync
            updater_state
    respond_loop state msg_reader

-- | A special run-and-sync that runs before the respond loop gets started.
run_setup_cmd :: Cmd.CmdIO -> State -> IO State
run_setup_cmd cmd state = fmap snd $ run_responder state $ do
    result <- run_continue "initial setup" $ Right $ do
        cmd
        State.update_all_tracks
        Cmd.modify $ \st -> st
            { Cmd.state_history = (Cmd.state_history st)
                { Cmd.hist_last_cmd = Just $ Cmd.Load Nothing ["setup"] }
            }
        return Cmd.Continue
    when_just result $ \(_, ui_state, cmd_state) -> do
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
create_msg_reader remap_rmsg midi_chan lang_socket ui_chan loopback_chan = do
    lang_chan <- TChan.newTChanIO
    Thread.start_logged "accept lang socket" $
        accept_loop lang_socket lang_chan
    return $ STM.atomically $
        fmap Msg.Ui (TChan.readTChan ui_chan)
        `STM.orElse` fmap (Msg.Midi . remap_rmsg) (TChan.readTChan midi_chan)
        `STM.orElse` fmap (uncurry Msg.Socket) (TChan.readTChan lang_chan)
        `STM.orElse` TChan.readTChan loopback_chan

-- | Accept a connection on the socket, read everything that comes over, then
-- place the socket and the read data on @output_chan@.  It's the caller's
-- responsibility to close the handle after it uses it to reply.
accept_loop :: Network.Socket -> TChan.TChan (IO.Handle, String) -> IO ()
accept_loop socket output_chan = forever $ catch_io_errors $ do
    (hdl, _host, _port) <- Network.accept socket
    IO.hSetBuffering hdl IO.NoBuffering
    msg <- read_until hdl Config.message_complete_token
    STM.atomically $ TChan.writeTChan output_chan (hdl, msg)
    where
    catch_io_errors = Exception.handle $ \(exc :: IOError) ->
        Log.warn $ "caught exception from socket read: " ++ show exc

read_until :: IO.Handle -> String -> IO String
read_until hdl boundary = go ""
    where
    rbound = reverse boundary
    go accum
        | rbound `List.isPrefixOf` accum =
            return (reverse (drop (length boundary) accum))
        | otherwise = do
            eof <- IO.hIsEOF hdl
            if eof then return "" else do
                c <- IO.hGetChar hdl
                go (c:accum)


-- * respond

respond_loop :: State -> MsgReader -> IO ()
respond_loop rstate msg_reader = do
    msg <- msg_reader
    -- putStrLn $ "msg: " ++ Pretty.pretty msg
    result <- Exception.try $ respond rstate msg
    case result of
        Left (exc :: Exception.SomeException) ->
            Log.error $ "exception caught in respond_loop: " ++ show exc
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
instance Error.Error Done
type Result = Either State.Error Cmd.Status

save_updates :: [Update.CmdUpdate] -> ResponderM ()
save_updates updates = Monad.State.modify $ \st ->
    st { rstate_updates = updates ++ (rstate_updates st) }

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
run_responder :: State -> ResponderM Result -> IO (Bool, State)
run_responder state m = do
    (val, (RState _ ui_from ui_to cmd_from cmd_to cmd_updates))
        <- Monad.State.runStateT m (make_rstate state)
    case val of
        Left err -> do
            Log.warn (Pretty.pretty err)
            -- Exception rolls back changes to ui_state and cmd_state.
            return (False, state { state_ui = ui_from, state_cmd = cmd_from })
        Right status -> do
            case status of
                Cmd.Play args -> Trans.liftIO $
                    PlayC.start_updater (state_transport_info state) args
                _ -> return ()
            cmd_to <- return $ fix_cmd_state ui_to cmd_to
            (updates, ui_to, cmd_to) <-
                ResponderSync.sync (state_sync state)
                    (send_derive_status (state_loopback state))
                    (state_ui state) ui_from ui_to cmd_to cmd_updates
                    (Transport.info_state (state_transport_info state))
            cmd_to <- Undo.maintain_history ui_to cmd_to updates
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

respond :: State -> Msg.Msg -> IO (Bool, State)
respond state msg = run_responder state $ do
    record_keys msg
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
    when_just result $ \(_, _, cmd_state) -> Monad.State.modify $ \st ->
        st { rstate_cmd_from = cmd_state, rstate_cmd_to = cmd_state }

-- | Record 'UiMsg.UiUpdate's from the UI.  Like normal cmds it can abort
-- processing by returning not-Continue, but it commits its changes to ui_from
-- instead of ui_to.  This means these changes don't participate in the diff
-- and sync.  This is because UiUpdates are reporting changes that already
-- happened, so they can't be rolled back and sending them back to the UI
-- would be silly.
record_ui_updates :: Msg.Msg -> ErrorResponderM ()
record_ui_updates msg = do
    (result, cmd_state) <- Trans.lift $ run_cmd $ Left $
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
        Internal.cmd_sync_status (rstate_ui_from rstate)
            (rstate_cmd_from rstate)
    when_just result $ \(_, ui_state, cmd_state) -> Monad.State.modify $ \st ->
        st { rstate_ui_to = ui_state, rstate_cmd_to = cmd_state }

-- ** core cmds

run_core_cmds :: Msg.Msg -> ErrorResponderM ()
run_core_cmds msg = do
    state <- Trans.lift $ Monad.State.gets rstate_state
    mapM_ (run_throw . Right . ($msg))
        (StaticConfig.global_cmds (state_static_config state))
    -- Focus commands and the rest of the pure commands come first so text
    -- entry can override io bound commands.
    let pure_cmds = hardcoded_cmds ++ GlobalKeymap.pure_cmds
    mapM_ (run_throw . Left . ($msg)) pure_cmds

    let config = state_static_config state
    -- Certain commands require IO.  Rather than make everything IO,
    -- I hardcode them in a special list that gets run in IO.
    let io_cmds = hardcoded_io_cmds (state_transport_info state)
                (state_session state) (StaticConfig.local_lang_dirs config)
    mapM_ (run_throw . Right . ($msg)) io_cmds

-- | Everyone always gets these commands.
hardcoded_cmds :: [Cmd.Cmd]
hardcoded_cmds =
    [Track.track_cmd, Internal.cmd_update_ui_state, Internal.cmd_record_focus]

-- | And these special commands that run in IO.
hardcoded_io_cmds :: Transport.Info -> Lang.Session -> [FilePath]
    -> [Msg.Msg -> Cmd.CmdIO]
hardcoded_io_cmds transport_info lang_session lang_dirs =
    [ Lang.cmd_language lang_session lang_dirs
    , Lilypond.cmd_compile, Integrate.cmd_integrate
    , PlayC.cmd_play_msg
    ] ++ GlobalKeymap.io_cmds transport_info

-- ** run cmds

type EitherCmd = Either (Cmd.CmdId Cmd.Status) Cmd.CmdIO
type ErrorResponderM = Error.ErrorT Done ResponderM

run_continue :: String -> EitherCmd
    -> ResponderM (Maybe (Cmd.Status, State.State, Cmd.State))
run_continue caller cmd = do
    (result, cmd_state) <- run_cmd cmd
    case result of
        Left err -> do
            Trans.liftIO $ Log.error $ caller ++ ": " ++ Pretty.pretty err
            return Nothing
        Right (status, ui_state) -> do
            when (not_continue status) $ Trans.liftIO $
                Log.error $ caller ++ ": " ++ "expected Continue: "
                    ++ show status
            return $ Just (status, ui_state, cmd_state)

run_throw :: EitherCmd -> ErrorResponderM ()
run_throw cmd = do
    (result, cmd_state) <- Trans.lift $ run_cmd cmd
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
    (cmd_state, midi, logs, result) <- Trans.liftIO $ case cmd of
        Left cmd ->
            Cmd.run_id_io (rstate_ui_to rstate) (rstate_cmd_to rstate) cmd
        Right cmd ->
            Cmd.run_io (rstate_ui_to rstate) (rstate_cmd_to rstate) cmd
    Trans.liftIO $ do
        mapM_ Log.write logs
        mapM_ (Cmd.state_midi_writer (rstate_cmd_to rstate))
            [Midi.WriteMessage dev 0 msg | (dev, msg) <- midi]
    case result of
        Left err -> return (Left err, cmd_state)
        Right (status, ui_state, updates) -> do
            save_updates updates
            return (Right (status, ui_state), cmd_state)

not_continue :: Cmd.Status -> Bool
not_continue Cmd.Continue = False
not_continue _ = True
