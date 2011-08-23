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
import qualified Control.Monad.Cont as Cont
import qualified Control.Monad.Trans as Trans

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Network
import qualified System.IO as IO

import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty
import qualified Util.Thread as Thread

import qualified Midi.Midi as Midi
import Ui
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Internal as Internal
import qualified Cmd.Lang as Lang
import qualified Cmd.Msg as Msg
import qualified Cmd.Play as Play
import qualified Cmd.ResponderSync as ResponderSync
import qualified Cmd.Track as Track
import qualified Cmd.Undo as Undo

import qualified Perform.Transport as Transport
import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig


data State = State {
    state_static_config :: StaticConfig.StaticConfig
    , state_ui :: State.State
    , state_cmd :: Cmd.State
    , state_midi_writer :: MidiWriter
    -- | Data needed by the performer threads.
    , state_transport_info :: Transport.Info
    -- | State for the lang subsystem.
    , state_session :: Lang.Session
    -- | This is used to feed msgs back into the MsgReader.
    , state_loopback :: Loopback
    -- | This function takes diffs and actually applies them to the UI.  It's
    -- passed as an argument so tests can run the responder without a UI.
    , state_sync :: ResponderSync.Sync
    }

type MidiWriter = Midi.WriteMessage -> IO ()
type MsgReader = IO Msg.Msg
type Loopback = Msg.Msg -> IO ()

responder :: StaticConfig.StaticConfig -> MsgReader -> MidiWriter
    -> IO () -> IO RealTime -> Cmd.CmdIO
    -> Lang.Session -> Loopback -> IO ()
responder static_config msg_reader write_midi abort_midi get_now setup_cmd
        lang_session loopback = do
    Log.debug "start responder"
    -- Report keymap overlaps.
    mapM_ Log.warn GlobalKeymap.cmd_map_errors

    let cmd_state = Cmd.initial_state
            (StaticConfig.config_instrument_db static_config)
            (StaticConfig.config_schema_map static_config)
            (StaticConfig.config_global_scope static_config)
        cmd = setup_cmd >> Edit.initialize_state >> return Cmd.Done
    updater_state <- MVar.newMVar State.empty
    (ui_state, cmd_state) <-
        run_setup_cmd loopback State.empty cmd_state updater_state cmd
    let rstate = State static_config ui_state cmd_state write_midi
            (Transport.Info send_status write_midi abort_midi get_now
                updater_state)
            lang_session loopback Sync.sync
    respond_loop rstate msg_reader
    where
    send_status = loopback . Msg.Transport

-- | A special run-and-sync that runs before the respond loop gets started.
run_setup_cmd :: Loopback -> State.State -> Cmd.State
    -> MVar.MVar State.State -> Cmd.CmdIO -> IO (State.State, Cmd.State)
run_setup_cmd loopback ui_state cmd_state updater_state cmd = do
    (cmd_state, _, logs, result) <- Cmd.run_io ui_state cmd_state cmd
    mapM_ Log.write logs
    (ui_to, updates) <- case result of
        Left err -> do
            Log.error $ "initial setup: " ++ show err
            return (ui_state, [])
        Right (status, ui_state, updates) -> do
            when (status /= Cmd.Done) $
                Log.warn $ "setup_cmd not Done: " ++ show status
            return (ui_state, updates)
    (_, ui_state, cmd_state) <-
        ResponderSync.sync Sync.sync (send_derive_status loopback)
            ui_state ui_state ui_to cmd_state updates updater_state
    return (ui_state, cmd_state)

send_derive_status :: Loopback -> BlockId -> Msg.DeriveStatus -> IO ()
send_derive_status loopback block_id status =
    loopback (Msg.DeriveStatus block_id status)

respond_loop :: State -> MsgReader -> IO ()
respond_loop rstate msg_reader = do
    msg <- msg_reader
    (quit, rstate) <- respond rstate msg
    unless quit (respond_loop rstate msg_reader)

-- | Create the MsgReader to pass to 'responder'.
create_msg_reader ::
    (Midi.ReadMessage -> Midi.ReadMessage) -> TChan.TChan Midi.ReadMessage
    -> Network.Socket -> TChan.TChan UiMsg.UiMsg -> TChan.TChan Msg.Msg
    -> IO MsgReader
create_msg_reader remap_rmsg midi_chan lang_socket ui_chan loopback_chan = do
    lang_chan <- TChan.newTChanIO
    Thread.start_logged "accept lang socket" (accept_loop lang_socket lang_chan)
    return $ STM.atomically $
        fmap Msg.Ui (TChan.readTChan ui_chan)
        `STM.orElse` fmap (Msg.Midi . remap_rmsg) (TChan.readTChan midi_chan)
        `STM.orElse` fmap (uncurry Msg.Socket) (TChan.readTChan lang_chan)
        `STM.orElse` TChan.readTChan loopback_chan

-- | Accept a connection on the socket, read everything that comes over, then
-- place the socket and the read data on @output_chan@.  It's the caller's
-- responsibility to close the handle after it uses it to reply.
accept_loop socket output_chan = forever $ catch_io_errors $ do
    (hdl, _host, _port) <- Network.accept socket
    IO.hSetBuffering hdl IO.NoBuffering
    msg <- read_until hdl Config.message_complete_token
    STM.atomically $ TChan.writeTChan output_chan (hdl, msg)

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


-- | (cstate, Either error (status, ui_from, ui_to))
type RType = (Either State.StateError (Cmd.Status, State.State, State.State),
    Cmd.State)
type ResponderM a = Cont.ContT RType (Logger.LoggerT Update.Update IO) a

run_responder :: ResponderM RType -> IO (RType, [Update.Update])
run_responder = Logger.run . flip Cont.runContT return

{- | The flow control makes this all way more complicated than I want it to be.
    There must be a simpler way.

    The responder is conceptually simple: get a new msg, then call each cmd
    with it.  If the cmd returns Abort or Continue, go to the next, if it
    returns Done, repeat the loop, and if it returns Quit, break out of the
    loop.  Then pass the ui state before all of this along with the final ui
    state to Sync so the UI can be updated.  The old ui state is also passed
    to 'Undo.record_history' for undo recording.

    The complications:

    1. Ui state and cmd state should be threaded through the calls if they
    return Continue but not Abort: Continue means it modified the state and
    wants to keep it, Abort means it doesn't want to keep it.

    2. There are a set of Cmds which must be run in IO, the rest can be run in
    Identity.

    3. There is also a Cmd which records change notifications from the UI, and
    therefore the changes it makes to the ui state should *not* be included in
    the final sync.

    4. The key down recording Cmd should be run at the end regardless of
    whether a previous Cmd was Done or Aborted.

    5. Auxiliary Cmd output, like logs, should be written after the Cmd
    returns.

    Implementing everything (ui syncs, key recording, midi_thru, etc.) as a Cmd
    is conceptually simple, but leads to complications here because they still
    have to be called differently.  I think even if I hardcoded some of those
    things it wouldn't make calling any simpler, because I would still have to
    call with them and deal with their results.

    To try to control some of this, I split the responder into multiple
    functions, but to control *that* I need continuations for early escape from
    nested calls.

    TODO: Give this some serious thought some day.  Also profile it.
-}
respond :: State -> Msg.Msg -> IO (Bool, State)
respond rstate msg = do
    -- putStrLn $ "msg: " ++ Pretty.pretty msg
    ((res, cmd_state), cmd_updates) <- run_responder (run_cmds rstate msg)
    rstate <- return $ rstate { state_cmd = cmd_state }
    (status, rstate) <- case res of
        Left err -> do
            Log.warn $ "responder: " ++ Pretty.pretty err
            return (Cmd.Continue, rstate)
        Right (status, ui_from, ui_to) -> do
            cmd_state <- return $ fix_cmd_state ui_to cmd_state
            (all_updates, ui_state, cmd_state) <-
                ResponderSync.sync (state_sync rstate)
                    (send_derive_status (state_loopback rstate))
                    (state_ui rstate) ui_from ui_to cmd_state cmd_updates
                    (Transport.info_state (state_transport_info rstate))
            cmd_state <- return $
                Undo.record_history cmd_updates all_updates ui_from cmd_state
            return (status,
                rstate { state_cmd = cmd_state, state_ui = ui_state })
    return (status == Cmd.Quit, rstate)

-- | If the focused view is removed, cmd state should stop pointing to it.
fix_cmd_state :: State.State -> Cmd.State -> Cmd.State
fix_cmd_state ui_state cmd_state = case Cmd.state_focused_view cmd_state of
    Just focus | focus `Map.notMember` State.state_views ui_state ->
        cmd_state { Cmd.state_focused_view = Nothing }
    _ -> cmd_state

run_cmds :: State -> Msg.Msg -> ResponderM RType
run_cmds rstate msg = do
    cstate <- record_keys rstate msg
    (result, cmd_state) <- Cont.callCC $ \exit ->
        run_core_cmds (rstate { state_cmd = cstate }) msg exit
    cmd_state <- return $
        either (const (state_cmd rstate)) (const cmd_state) result
    return $ case result of
        Left _ -> (result, cmd_state)
        Right (status, ui_from, ui_to) ->
            (Right (status, ui_from, ui_to), cmd_state)

-- | Run the record keys cmd separately.  It would be nicer just to stick it
-- on the front of the cmd list, but then if one of the cmds threw an
-- exception, the key recording would also be reverted.
record_keys :: State -> Msg.Msg -> ResponderM Cmd.State
record_keys rstate msg = do
    Trans.liftIO $ do
        mapM_ Log.write logs
        case result of
            Left err -> Log.error ("record keys error: " ++ show err)
            _ -> return ()
    return cstate
    where
    (cstate, _, logs, result) = Cmd.run_id
        (state_ui rstate) (state_cmd rstate) (Internal.cmd_record_keys msg)

run_core_cmds :: State -> Msg.Msg
    -> (RType -> ResponderM (State.State, Cmd.State)) -> ResponderM RType
run_core_cmds rstate msg exit = do
    let ui_from = state_ui rstate
        cmd_state = state_cmd rstate

    -- Run ui records first so they can't get aborted by other cmds.
    (ui_from, cmd_state) <- do_run exit Cmd.run_id_io rstate msg ui_from
        ui_from cmd_state [Internal.cmd_record_ui_updates]
    let ui_to = ui_from

    -- Focus commands and the rest of the pure commands come first so text
    -- entry can override io bound commands.
    let pure_cmds =
            StaticConfig.config_global_cmds (state_static_config rstate)
            ++ hardcoded_cmds ++ GlobalKeymap.pure_cmds
    (ui_to, cmd_state) <- do_run exit Cmd.run_id_io rstate msg ui_from
        ui_to cmd_state pure_cmds

    let config = state_static_config rstate
    -- Certain commands require IO.  Rather than make everything IO,
    -- I hardcode them in a special list that gets run in IO.
    let io_cmds = hardcoded_io_cmds (state_transport_info rstate)
                (state_session rstate)
                (StaticConfig.config_local_lang_dirs config)
    (ui_to, cmd_state) <- do_run exit Cmd.run_io rstate msg ui_from
        ui_to cmd_state io_cmds
    return (Right (Cmd.Continue, ui_from, ui_to), cmd_state)

-- | Everyone always gets these commands.
hardcoded_cmds :: [Cmd.Cmd]
hardcoded_cmds =
    [Track.track_cmd, Internal.cmd_update_ui_state, Internal.cmd_record_focus]

-- | And these special commands that run in IO.
hardcoded_io_cmds :: Transport.Info -> Lang.Session -> [FilePath]
    -> [Msg.Msg -> Cmd.CmdIO]
hardcoded_io_cmds transport_info lang_session lang_dirs =
    [ Lang.cmd_language lang_session lang_dirs
    , Play.cmd_play_msg
    ] ++ GlobalKeymap.io_cmds transport_info

-- | ui_from is needed since this can abort with an RType as soon as it gets
-- a non Continue status.
do_run :: (Monad m) => (RType -> ResponderM (State.State, Cmd.State))
    -> Cmd.RunCmd m IO Cmd.Status
    -> State -> Msg.Msg -> State.State -> State.State -> Cmd.State
    -> [Msg.Msg -> Cmd.CmdT m Cmd.Status]
    -> ResponderM (State.State, Cmd.State)
do_run exit runner rstate msg ui_from ui_state cmd_state cmds = do
    res <- Trans.liftIO $ run_cmd_list [] (state_midi_writer rstate) ui_state
        cmd_state runner (map ($msg) cmds)
    case res of
        Right (Cmd.Continue, ui_state, cmd_state, updates) -> do
            Trans.lift $ Logger.logs updates
            return (ui_state, cmd_state)
        Right (status, ui_state, cmd_state, updates) -> do
            Trans.lift $ Logger.logs updates
            exit (Right (status, ui_from, ui_state), cmd_state)
        Left err -> exit (Left err, cmd_state)

run_cmd_list :: (Monad m) => [Update.Update] -> MidiWriter -> State.State
    -> Cmd.State -> Cmd.RunCmd m IO Cmd.Status -> [Cmd.CmdT m Cmd.Status]
    -> IO (Either State.StateError
        (Cmd.Status, State.State, Cmd.State, [Update.Update]))
run_cmd_list updates0 write_midi ui_state cmd_state runner (cmd:cmds) = do
    (cmd_state, midi, logs, ui_result) <- runner ui_state cmd_state cmd
    sequence_ [write_midi (Midi.WriteMessage dev 0 msg)
        | (dev, msg) <- midi]
    mapM_ Log.write logs
    case ui_result of
        Right (Cmd.Continue, ui_state, updates) -> run_cmd_list
            (updates0 ++ updates) write_midi ui_state cmd_state runner cmds
        Right (status, ui_state, updates) ->
            return $ Right (status, ui_state, cmd_state, updates0 ++ updates)
        Left err -> return $ Left err
run_cmd_list updates _ ui_state cmd_state _ [] =
    return $ Right (Cmd.Continue, ui_state, cmd_state, updates)
