{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | The responder is the main event loop on the haskell side.

    It receives msgs (described in Cmd.Msg) multiplexed through a set of
    channels which come from various sources: the UI event loop (in its own
    thread), a socket, the MIDI library, etc.  The Msgs are then dispatched
    through Cmds to treat as they will, stopping when one returns Cmd.Done.

    The responder then deals with the results of the Cmds: midi thru output is
    sent and the old state is diffed with the new state to produce Updates,
    which are given to Sync to sync the visible UI with the changes the Cmds
    made to the UI state.


    This is possibly not the best place to describe the signal display, but
    until I think of a better one:

    The signal display mechanism goes from the deriver to the UI, so it's
    spread across Schema, Derive, Responder, to Sync.  Effectively this is
    a path from the deriver to the UI, just as the "play" path is from the
    deriver to Perform.  Schema has a special "signal_deriver" field, and the
    signal deriver is compiled from the Skeleton in a way parallel the score
    event deriver.

    While the event deriver is invoked at play time, the signal deriver is
    invoked by the responder before syncing the state to the UI.  The deriver
    creates signal maps for each track for each block.  It's important that the
    signal maps remain lazy because only portions of them may be needed if
    there are only localized track updates (or none at all if there are no
    track updates).

    A track's samples are much like its events in that they are spread across
    the track's entire range but only displayed for the visible subset.
    However, while events have a special mechanism/hack to capture modified
    ranges (as described in Ui.State) so the display can be refreshed
    incrementally, samples have no such mechanism.  This is because they are
    expected to only change in reaction to events changing, and to only change
    within the range of the changed events.  Since an event alteration will
    emit Update.TrackEvents, the track should be redrawn in the given range and
    refresh the altered sample area.

    TODO I'm not entirely satisfied with this, elegance-wise or
    efficiency-wise.  I'll wait until I have experience with large tracks and
    dense samples before giving this more thought.  It seems like I should be
    able to render samples directly to a buffer which gets passed by pointer to
    c++.
-}
module Cmd.Responder where

import qualified Control.Arrow as Arrow
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Language.Haskell.Interpreter.GHC as GHC
import qualified Network
import qualified System.IO as IO

import qualified Util.Data
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Track as Track
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update
import qualified Midi.Midi as Midi
import qualified Perform.Transport as Transport
import qualified Perform.Timestamp as Timestamp

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Language as Language
import qualified Cmd.Msg as Msg
import qualified Cmd.DefaultKeymap as DefaultKeymap
import qualified Cmd.Play as Play
import qualified Cmd.Save as Save

import qualified Derive.Derive as Derive
import qualified Derive.Schema as Schema

import qualified Perform.Signal as Signal

import qualified App.Config as Config
import qualified App.StaticConfig as StaticConfig


type MidiWriter = Midi.WriteMessage -> IO ()
type MsgReader = IO Msg.Msg

responder :: StaticConfig.StaticConfig -> MsgReader -> MidiWriter
    -> IO Timestamp.Timestamp -> Transport.Chan -> Cmd.CmdIO
    -> GHC.InterpreterSession -> IO ()
responder static_config get_msg write_midi get_ts player_chan setup_cmd
        session = do
    Log.debug "start responder"

    let ui_state = State.empty
        cmd_state = Cmd.initial_state
            (StaticConfig.config_instrument_db static_config)
            (StaticConfig.config_schema_map static_config)
        cmd = setup_cmd >> Save.initialize_state >> return Cmd.Done
    (status, ui_state, cmd_state) <- do
        cmd_val <- run_cmds (Cmd.run Cmd.Continue) ui_state cmd_state [cmd]
        handle_cmd_val "initial setup" True write_midi ui_state cmd_val
    when (status /= Cmd.Done) $
        Log.warn $ "setup_cmd returned not-Done status, did it abort?"
    let rstate = ResponderState static_config ui_state cmd_state get_msg
            write_midi (Transport.Info player_chan write_midi get_ts)
            session
    loop rstate

-- | Create the MsgReader to pass to 'responder'.
create_msg_reader ::
    Map.Map Midi.ReadDevice Midi.ReadDevice -> TChan.TChan Midi.ReadMessage
    -> Network.Socket -> TChan.TChan UiMsg.UiMsg -> Transport.Chan
    -> IO MsgReader
create_msg_reader rdev_map midi_chan lang_socket ui_chan player_chan = do
    lang_chan <- TChan.newTChanIO
    Thread.start_thread "accept lang socket" (accept_loop lang_socket lang_chan)
    let map_dev rmsg@(Midi.ReadMessage { Midi.rmsg_dev = dev }) =
            rmsg { Midi.rmsg_dev = Util.Data.get dev dev rdev_map }
    return $ STM.atomically $
        fmap Msg.Ui (TChan.readTChan ui_chan)
        `STM.orElse` fmap (Msg.Midi . map_dev) (TChan.readTChan midi_chan)
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

catch_io_errors = Exception.handleJust Exception.ioErrors $ \exc -> do
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
hardcoded_io_cmds play_info session lang_dirs =
    [ Language.cmd_language session lang_dirs
    , Play.cmd_transport_msg
    , DefaultKeymap.cmd_io_keymap play_info
    ]

data ResponderState = ResponderState {
    state_static_config :: StaticConfig.StaticConfig
    , state_ui :: State.State
    , state_cmd :: Cmd.State
    , state_msg_reader :: MsgReader
    , state_midi_writer :: MidiWriter
    , state_transport_info :: Transport.Info
    , state_ghc_session :: GHC.InterpreterSession
    }

loop :: ResponderState -> IO ()
loop rstate = do
    msg <- state_msg_reader rstate
    -- Apply changes that won't be diffed.  See the 'Cmd.cmd_record_ui_updates'
    -- comment.
    -- TODO: an error implies the UI is out of sync, maybe I should fail more
    -- seriously here?
    let update_cmds = map ($msg) [Cmd.cmd_record_ui_updates]
        ui_state = state_ui rstate
        cmd_state = state_cmd rstate
        write_midi = state_midi_writer rstate
    (status, ui_state, cmd_state) <- do
        cmd_val <- run_cmds Cmd.run_id_io ui_state cmd_state update_cmds
        handle_cmd_val "record ui updates" False write_midi ui_state cmd_val

    let config = state_static_config rstate

    -- Certain commands require IO.  Rather than make everything IO,
    -- I hardcode them in a special list that gets run in IO.
    let play_info =
            ( StaticConfig.config_instrument_db config
            , state_transport_info rstate
            , StaticConfig.config_schema_map config
            )
    let io_cmds = StaticConfig.config_global_cmds config
            ++ hardcoded_io_cmds play_info (state_ghc_session rstate)
                (StaticConfig.config_local_lang_dirs config)
    (status, ui_state, cmd_state) <- maybe_run "run io cmds" status write_midi
        ui_state cmd_state (Cmd.run Cmd.Continue) io_cmds msg

    focus_cmds <- eval "get focus cmds" ui_state cmd_state [] get_focus_cmds
    let id_cmds = hardcoded_cmds ++ focus_cmds ++ DefaultKeymap.default_cmds
    (status, ui_state, cmd_state) <- maybe_run "run pure cmds" status write_midi
        ui_state cmd_state Cmd.run_id_io id_cmds msg

    -- Record the keys last, so they show up in the next cycle's keys_down,
    -- but not this one.  This is so you can tell the difference between a
    -- key down and a key repeat.
    (_, ui_state, cmd_state) <- maybe_run "record keys" Cmd.Continue write_midi
        ui_state cmd_state Cmd.run_id_io [Cmd.cmd_record_keys] msg

    let rstate2 = rstate { state_ui = ui_state, state_cmd = cmd_state }
    case status of
        Cmd.Quit -> return ()
        _ -> loop rstate2

-- | Run the cmds on msg if @status@ is Continue.
maybe_run err_msg status write_midi ui_state cmd_state run cmds msg =
    case status of
        Cmd.Continue -> do
            cmd_val <- run_cmds run ui_state cmd_state (map ($msg) cmds)
            handle_cmd_val err_msg True write_midi ui_state cmd_val
        _ -> return (status, ui_state, cmd_state)

-- TODO
-- type RunInfo = (MidiWriter, State.State, Cmd.State, Track.TrackSamples)

-- | Run the given list of Cmds against the Msg, stopping and returning as soon
-- as one doesn't return Continue.  Use the given @run@ function to run the
-- Cmd, since it may be in either IO or Identity.
run_cmds :: (Monad cmd_m, Monad val_m) =>
    Cmd.RunCmd cmd_m val_m Cmd.Status -- ^ run the cmd's result monad
    -> State.State -> Cmd.State -- ^ initial states for the cmd
    -> [Cmd.CmdT cmd_m Cmd.Status] -- ^ cmd list
    -> val_m (Cmd.CmdVal Cmd.Status)
run_cmds _run ui_state cmd_state [] =
    -- CmdVals are pretty complicated...
    return (cmd_state, [], [], Right (Cmd.Continue, ui_state, []))
run_cmds run ui_state cmd_state (cmd:cmds) = do
    (cmd_state1, midi1, logs1, ui_res1) <- run ui_state cmd_state cmd
    case ui_res1 of
        Right (Cmd.Continue, ui_state1, updates1) -> do
            (cmd_state2, midi2, logs2, ui_res2) <-
                run_cmds run ui_state1 cmd_state1 cmds
            return (cmd_state2, midi1 ++ midi2, logs1 ++ logs2,
                merge_ui_res updates1 ui_res2)
        Left err -> return (cmd_state1, midi1, logs1, Left err)
        -- It's Quit or Done, so return as-is.
        _ -> return (cmd_state1, midi1, logs1, ui_res1)

handle_cmd_val :: String -> Bool -> MidiWriter -> State.State
    -> Cmd.CmdVal Cmd.Status -> IO (Cmd.Status, State.State, Cmd.State)
handle_cmd_val err_msg do_sync write_midi ui_state1
        (cmd_state, midi, logs, ui_result) = do
    sequence_ [write_midi (Midi.WriteMessage dev Timestamp.immediately msg)
        | (dev, msg) <- midi]
    mapM_ Log.write logs
    (status, ui_state2) <- case ui_result of
        Left err -> do
            Log.error $ "ui error in " ++ show err_msg ++ ": " ++ show err
            return (Cmd.Done, ui_state1)
        Right (status, ui_state2, cmd_updates) -> do
            if do_sync
                then do
                    ui_state2 <- sync (Cmd.state_schema_map cmd_state)
                        ui_state1 ui_state2 cmd_updates
                    return (status, ui_state2)
                else return (status, ui_state2)
    return (status, ui_state2, cmd_state)

-- | Get cmds according to the currently focused block and track.
get_focus_cmds :: Cmd.CmdT Identity.Identity [Cmd.Cmd]
get_focus_cmds = do
    block <- State.block_of_view =<< Cmd.get_focused_view
    tracks <- Schema.block_tracks block
    midi_config <- State.get_midi_config
    edit_mode <- fmap Cmd.state_edit_mode Cmd.get_state
    tracknum <- Cmd.get_insert_tracknum

    let context = Schema.cmd_context midi_config edit_mode tracknum
    schema_map <- Cmd.get_schema_map
    return $
        Schema.get_cmds schema_map context (Block.block_schema block) tracks


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

-- Merge updates into a StateT result.
merge_ui_res updates = fmap
    (\(status, ui_state, updates2) -> (status, ui_state, updates ++ updates2))

-- | This should be run before every sync, since if errors get to sync they'll
-- result in bad UI display, a C++ exception, or maybe even a segfault (but C++
-- args should be protected by ASSERTs).
--
-- If there was any need, Cmd.State verification could go here too.
-- verify_state :: (Monad m) => Cmd.CmdT m ()
verify_state state = do
    let (res, logs) = State.verify state
    mapM_ Log.write logs
    case res of
        Left err -> error $ "fatal state consistency error: " ++ show err
        Right state2 -> return state2

-- | Sync @state2@ to the UI.
sync :: Schema.SchemaMap -> State.State -> State.State -> [Update.Update]
    -> IO State.State
sync schema_map state1 state2 cmd_updates = do
    -- I'd catch problems closer to their source if I did this from run_cmds,
    -- but it's nice to see that it's definitely happening before syncs.
    state2 <- verify_state state2
    let (block_samples, sig_logs) = derive_signals schema_map state2
    -- Don't want to force block_samples because it computes a lot of stuff
    -- that sync may never need.  TODO how can I lazily write log msgs?
    -- Actually, the expensive part is the sampling, which has no logging,
    -- so it should remain lazy... I think.  Verify this.
    mapM_ Log.write sig_logs
    -- putStrLn $ "block samples: " ++ show block_samples
    case Diff.diff state1 state2 of
        Left err -> Log.error $ "diff error: " ++ err
        Right diff_updates -> do
            unless ((null diff_updates) && (null cmd_updates)) $
                Log.debug $ "diff_updates: " ++ show diff_updates
                    ++ " cmd_updates: " ++ show cmd_updates
            err <- Sync.sync state2 (diff_updates ++ cmd_updates) block_samples
            case err of
                Nothing -> return ()
                Just err -> Log.error $ "syncing updates: " ++ show err
    return state2

derive_signals :: Schema.SchemaMap -> State.State
    -> (Sync.BlockSamples, [Log.Msg])
derive_signals schema_map ui_state = (block_samples, logs)
    where
    block_ids = Map.keys (State.state_blocks ui_state)
    track_results = zip block_ids $
        map (State.eval ui_state . derive_signal schema_map) block_ids

    block_samples = [(block_id, track_samples)
        | (block_id, Right (track_samples, _)) <- track_results]
    logs = [warn block_id err | (block_id, Left err) <- track_results]
        ++ concat [logs | (_, Right (_, logs)) <- track_results]
    warn block_id err = Log.msg Log.Warn $
        "exception deriving signal for " ++ show block_id ++ ": " ++ show err

derive_signal :: (State.UiStateMonad m) =>
    Schema.SchemaMap -> Block.BlockId -> m (Track.TrackSamples, [Log.Msg])
derive_signal schema_map block_id = do
    ui_state <- State.get
    deriver <- Schema.get_signal_deriver schema_map =<< State.get_block block_id
    let (result, _, _, logs, _) = Derive.derive
            Derive.empty_lookup_deriver ui_state True
            (Derive.with_stack_block block_id deriver)
    case result of
        Left err -> State.throw (show err)
        Right sig ->
            return (map (Arrow.second Signal.to_track_samples) sig, logs)


-- * util

-- | Like 'sequence_', but stop sequencing when @pred@ is false, and return
-- that value.  Return @zero@ if @pred@ is never false.
sequence_while :: (Monad m) => (a -> Bool) -> a -> [m a] -> m a
sequence_while _pred zero [] = return zero
sequence_while pred zero (op:ops) = do
    val <- op
    if pred val
        then sequence_while pred zero ops
        else return val
