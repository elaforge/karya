{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- ghc confused about Control.Monad
{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
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
module Cmd.Responder where

import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Cont as Cont
import qualified Control.Monad.Trans as Trans
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Network
import qualified System.IO as IO

import qualified Util.Logger as Logger
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update
import qualified Midi.Midi as Midi
import qualified Perform.Transport as Transport
import qualified Perform.Timestamp as Timestamp

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.GlobalKeymap as GlobalKeymap
import qualified Cmd.Language as Language
import qualified Cmd.Msg as Msg
import qualified Cmd.Play as Play
import qualified Cmd.ResponderSync as ResponderSync

import qualified Derive.Schema as Schema

import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig


data ResponderState = ResponderState {
    state_static_config :: StaticConfig.StaticConfig
    , state_ui :: State.State
    , state_cmd :: Cmd.State
    , state_msg_reader :: MsgReader
    , state_midi_writer :: MidiWriter
    , state_transport_info :: Transport.Info
    , state_interpreter_chan :: Language.InterpreterChan
    }

type MidiWriter = Midi.WriteMessage -> IO ()
type MsgReader = IO Msg.Msg

responder :: StaticConfig.StaticConfig -> MsgReader -> MidiWriter
    -> IO () -> IO Timestamp.Timestamp -> Transport.Chan -> Cmd.CmdIO
    -> Language.InterpreterChan -> IO ()
responder static_config get_msg write_midi abort_midi get_now_ts player_chan
        setup_cmd interpreter_chan = do
    Log.debug "start responder"
    -- Report keymap overlaps.
    mapM_ Log.warn
        (GlobalKeymap.io_cmd_map_errors ++ GlobalKeymap.cmd_map_errors)

    let cmd_state = Cmd.initial_state
            (StaticConfig.config_instrument_db static_config)
            (StaticConfig.config_schema_map static_config)
        cmd = setup_cmd >> Edit.initialize_state >> return Cmd.Done
    (ui_state, cmd_state) <- run_setup_cmd State.empty cmd_state cmd
    let rstate = ResponderState static_config ui_state cmd_state
            get_msg write_midi
            (Transport.Info player_chan write_midi abort_midi get_now_ts)
            interpreter_chan
    respond_loop rstate

-- | A special run-and-sync that runs before the respond loop gets started.
run_setup_cmd :: State.State -> Cmd.State -> Cmd.CmdIO
    -> IO (State.State, Cmd.State)
run_setup_cmd ui_state cmd_state cmd = do
    (cmd_state, _, logs, result) <- Cmd.run_io ui_state cmd_state cmd
    mapM_ Log.write logs
    (ui_to, updates) <- case result of
        Left err -> do
            Log.error $ "initial setup: " ++ show err
            return (ui_state, [])
        Right (status, ui_state, updates) -> do
            when (status /= Cmd.Done) $
                Log.warn $ "setup_cmd returned not-Done status, did it abort?"
            return (ui_state, updates)
    (_, ui_state, cmd_state) <-
        ResponderSync.sync ui_state ui_to cmd_state updates
    return (ui_state, cmd_state)

respond_loop :: ResponderState -> IO ()
respond_loop rstate = do
    Log.timer "---------- responder loop"
    (quit, rstate) <- respond rstate
    when quit (respond_loop rstate)

-- | Create the MsgReader to pass to 'responder'.
create_msg_reader ::
    (Midi.ReadMessage -> Midi.ReadMessage) -> TChan.TChan Midi.ReadMessage
    -> Network.Socket -> TChan.TChan UiMsg.UiMsg -> Transport.Chan
    -> IO MsgReader
create_msg_reader remap_rmsg midi_chan lang_socket ui_chan player_chan = do
    lang_chan <- TChan.newTChanIO
    Thread.start_thread "accept lang socket" (accept_loop lang_socket lang_chan)
    return $ STM.atomically $
        fmap Msg.Ui (TChan.readTChan ui_chan)
        `STM.orElse` fmap (Msg.Midi . remap_rmsg) (TChan.readTChan midi_chan)
        `STM.orElse` fmap Msg.Transport (TChan.readTChan player_chan)
        `STM.orElse` fmap (uncurry Msg.Socket) (TChan.readTChan lang_chan)

-- | Accept a connection on the socket, read everything that comes over, then
-- place the socket and the read data on @output_chan@.  It's the caller's
-- responsibility to close the handle after it uses it to reply.
accept_loop socket output_chan = forever $ catch_io_errors $ do
    (hdl, _host, _port) <- Network.accept socket
    IO.hSetBuffering hdl IO.NoBuffering
    msg <- read_until hdl Config.message_complete_token
    STM.atomically $ TChan.writeTChan output_chan (hdl, msg)

catch_io_errors = Exception.handle $ \(exc :: IOError) -> do
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
    state to Sync so the UI can be updated.  The old ui state is also passed to
    'record_history' for undo recording.

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
respond :: ResponderState -> IO (Bool, ResponderState)
respond rstate = do
    msg <- state_msg_reader rstate
    Log.timer $ "received msg: " ++ Msg.pretty_msg msg
    ((res, cmd_state), updates) <- run_responder (run_cmds rstate msg)
    rstate <- return $ rstate { state_cmd = cmd_state }
    (status, rstate) <- case res of
        Left err -> do
            Log.warn $ "responder: " ++ show err
            return (Cmd.Continue, rstate)
        Right (status, ui_from, ui_to) -> do
            Log.timer "syncing"
            cmd_state <- return $ fix_cmd_state ui_to cmd_state
            (updates, ui_state, cmd_state) <-
                ResponderSync.sync ui_from ui_to cmd_state updates
            cmd_state <- record_history updates ui_from cmd_state
            return (status,
                rstate { state_cmd = cmd_state, state_ui = ui_state })
    return (status /= Cmd.Quit, rstate)

-- | If the focused view is removed, cmd state should stop pointing to it.
fix_cmd_state :: State.State -> Cmd.State -> Cmd.State
fix_cmd_state ui_state cmd_state = case Cmd.state_focused_view cmd_state of
    Just focus | focus `Map.notMember` State.state_views ui_state ->
        cmd_state { Cmd.state_focused_view = Nothing }
    _ -> cmd_state

-- ** undo

-- Do the traditional thing where an action deletes the redo buffer.
-- At some point I could think about a real branching history, but not now.
record_history updates old_state cmd_state = do
    let cmd_name = "none yet"
        hist = fst (Cmd.state_history cmd_state)
    let record = not (Cmd.state_skip_history_record cmd_state)
            && should_record_history updates
        new_hist = if record
            then (Cmd.HistoryEntry cmd_name old_state : hist, [])
            else Cmd.state_history cmd_state
    -- let msg = (if record then "record " else "don't record ")
    --         ++ show (length (fst new_hist), length (snd new_hist))
    -- when record (Log.debug $ "history " ++ msg)
    return $ cmd_state
        { Cmd.state_history = new_hist, Cmd.state_skip_history_record = False }

-- TODO I'd like to be able to undo only non-view changes, leaving the view
-- where it is.  Or undo only the view changes, which means zoom and selection.
-- Or rather, view changes could be recorded in a separate undo history.
-- It would also be nice to only undo within a selected area.
should_record_history :: [Update.Update] -> Bool
should_record_history = any (not . Update.is_view_update)

run_cmds :: ResponderState -> Msg.Msg -> ResponderM RType
run_cmds rstate msg = do
    (result, cmd_state) <- Cont.callCC $ \exit -> run_core_cmds rstate msg exit
    -- Record the keys last, so they show up in the next cycle's keys_down,
    -- but not this one.  This is so you can tell the difference between a
    -- key down and a key repeat.
    -- To save me from stuck keys, this gets run even if the cmd throws.
    -- Changes made to cmd_state are discarded after an exception.
    -- This matches the behaviour of the ui state.
    cmd_state <- return $
        either (const (state_cmd rstate)) (const cmd_state) result
    -- Yeah, I pass the old ui state, but cmd_record_keys shouldn't be touching
    -- that anyway.
    let (recorded_cstate, _, rec_logs, rec_result) = Cmd.run_id
            (state_ui rstate) cmd_state (Cmd.cmd_record_keys msg)
    Trans.liftIO $ do
        mapM_ Log.write rec_logs
        case rec_result of
            Left err -> Log.error ("record keys error: " ++ show err)
            _ -> return ()
    return $ case result of
        Left _ -> (result, recorded_cstate)
        Right (status, ui_from, ui_to) ->
            (Right (status, ui_from, ui_to), recorded_cstate)

run_core_cmds :: ResponderState -> Msg.Msg
    -> (RType -> ResponderM (State.State, Cmd.State)) -> ResponderM RType
run_core_cmds rstate msg exit = do
    Trans.liftIO $ Log.timer "run core cmds"
    let ui_from = state_ui rstate
        cmd_state = state_cmd rstate
    (ui_from, cmd_state) <- do_run exit Cmd.run_id_io rstate msg ui_from
        ui_from cmd_state [Cmd.cmd_record_ui_updates]
    Trans.liftIO $ Log.timer "ran record updates"
    let ui_to = ui_from

    -- Focus commands and the rest of the pure commands come first so text
    -- entry can override io bound commands.
    focus_cmds <- Trans.liftIO $
        eval "get focus cmds" ui_to cmd_state [] get_focus_cmds
    Trans.liftIO $
        Log.timer ("ran get focus cmds: " ++ show (length focus_cmds))
    let id_cmds = focus_cmds ++ hardcoded_cmds ++ GlobalKeymap.global_cmds
    (ui_to, cmd_state) <- do_run exit Cmd.run_id_io rstate msg ui_from
        ui_to cmd_state id_cmds
    Trans.liftIO $ Log.timer "ran pure cmds"

    let config = state_static_config rstate
    -- Certain commands require IO.  Rather than make everything IO,
    -- I hardcode them in a special list that gets run in IO.
    let io_cmds = StaticConfig.config_global_cmds config
            ++ hardcoded_io_cmds (state_transport_info rstate)
                (state_interpreter_chan rstate)
                (StaticConfig.config_local_lang_dirs config)
    (ui_to, cmd_state) <- do_run exit Cmd.run_io rstate msg ui_from
        ui_to cmd_state io_cmds
    Trans.liftIO $ Log.timer "ran io cmds"

    return (Right (Cmd.Continue, ui_from, ui_to), cmd_state)

-- | Everyone always gets these commands.
hardcoded_cmds :: [Cmd.Cmd]
hardcoded_cmds =
    -- Special Cmds that record info about the incoming msgs.
    [ Cmd.cmd_update_ui_state, Cmd.cmd_record_active
    -- , Cmd.cmd_log
    -- Handle special case global msgs.
    , Cmd.cmd_close_window
    ]

-- | And these special commands that run in IO.
hardcoded_io_cmds transport_info interpreter_chan lang_dirs =
    [ Language.cmd_language interpreter_chan lang_dirs
    , Play.cmd_transport_msg
    ] ++ GlobalKeymap.io_cmds transport_info


-- | Get cmds according to the currently focused block and track.
get_focus_cmds :: Cmd.CmdT Identity.Identity [Cmd.Cmd]
get_focus_cmds = do
    block_id <- Cmd.get_focused_block
    midi_config <- State.get_midi_config
    tracknum <- Cmd.get_insert_tracknum
    lookup_midi <- Cmd.get_lookup_midi_instrument
    cmd_state <- Cmd.get_state
    track_tree <- State.get_track_tree block_id

    let context = Schema.cmd_context midi_config lookup_midi
            (Cmd.state_edit_mode cmd_state) (Cmd.state_kbd_entry cmd_state)
            tracknum track_tree
    schema_map <- Cmd.get_schema_map
    block <- State.get_block block_id
    return $ Schema.get_cmds schema_map context (Block.block_schema block)

-- | Run the cmd just for its value.
eval err_msg ui_state cmd_state abort_val cmd = do
    let (_, _, logs, ui_res) = Identity.runIdentity $
            Cmd.run abort_val ui_state cmd_state cmd
    mapM_ Log.write logs
    case ui_res of
        Right (val, _, _) -> return val
        Left err -> do
            Log.error $ "ui error in " ++ show err_msg ++ ": " ++ show err
            return abort_val

-- | ui_from is needed since this can abort with an RType as soon as it gets
-- a non Continue status.
do_run :: (Monad m) => (RType -> ResponderM (State.State, Cmd.State))
    -> Cmd.RunCmd m IO Cmd.Status
    -> ResponderState -> Msg.Msg -> State.State -> State.State -> Cmd.State
    -> [Msg.Msg -> Cmd.CmdM m] -> ResponderM (State.State, Cmd.State)
do_run exit runner rstate msg ui_from ui_state cmd_state cmds = do
    res <- Trans.liftIO $ run_cmd_list [] (state_midi_writer rstate) ui_state
        cmd_state runner (map ($msg) cmds)
    case res of
        Right (Cmd.Continue, ui_state, cmd_state, updates) -> do
            Trans.lift $ Logger.record_list updates
            return (ui_state, cmd_state)
        Right (status, ui_state, cmd_state, updates) -> do
            Trans.lift $ Logger.record_list updates
            exit (Right (status, ui_from, ui_state), cmd_state)
        Left err -> exit (Left err, cmd_state)

run_cmd_list :: (Monad m) => [Update.Update] -> MidiWriter -> State.State
    -> Cmd.State -> Cmd.RunCmd m IO Cmd.Status -> [Cmd.CmdM m]
    -> IO (Either State.StateError
        (Cmd.Status, State.State, Cmd.State, [Update.Update]))
run_cmd_list updates0 write_midi ui_state cmd_state runner (cmd:cmds) = do
    (cmd_state, midi, logs, ui_result) <- runner ui_state cmd_state cmd
    sequence_ [write_midi (Midi.WriteMessage dev Timestamp.immediately msg)
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
