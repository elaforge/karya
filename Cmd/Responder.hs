module Cmd.Responder where

import Control.Monad
import qualified Control.Exception as Exception
import qualified Control.Monad.Identity as Identity
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Language.Haskell.Interpreter.GHC as GHC
import qualified Network
import qualified System.IO as IO

import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Diff as Diff
import qualified Ui.Update as Update
import qualified Ui.UiMsg as UiMsg
import qualified Midi.Midi as Midi
import qualified Perform.Transport as Transport
import qualified Perform.Timestamp as Timestamp

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Language as Language
import qualified Cmd.Msg as Msg
import qualified Cmd.DefaultKeymap as DefaultKeymap
import qualified Cmd.Play as Play


type MidiWriter = Midi.WriteMessage -> IO ()
type MsgReader = IO Msg.Msg

responder :: MsgReader -> MidiWriter -> IO Timestamp.Timestamp -> Transport.Chan
    -> Cmd.CmdM -> IO ()
responder get_msg write_midi get_ts player_chan setup_cmd = do
    Log.notice "starting responder"
    let ui_state = State.empty
        cmd_state = Cmd.empty_state
    -- (_, ui_state, cmd_state) <- handle_cmd_val True write_midi ui_state
    --     (Cmd.run_cmd ui_state Cmd.empty_state setup_cmd)
    (_status, ui_state, cmd_state) <- do
        cmd_val <- run_cmds Cmd.run_cmd ui_state cmd_state [setup_cmd]
        handle_cmd_val True write_midi ui_state cmd_val
    session <- GHC.newSession
    Log.notice "done"
    loop ui_state cmd_state get_msg write_midi
        (Transport.Info player_chan write_midi get_ts)
        session

-- | Create the MsgReader to pass to 'responder'.
create_msg_reader :: Network.Socket -> TChan.TChan UiMsg.UiMsg
    -> TChan.TChan Midi.ReadMessage -> Transport.Chan -> IO MsgReader
create_msg_reader lang_socket ui_chan midi_chan player_chan = do
    lang_chan <- TChan.newTChanIO
    Thread.start_thread "accept lang socket" (accept_loop lang_socket lang_chan)
    return $ STM.atomically $
        fmap Msg.Ui (TChan.readTChan ui_chan)
        `STM.orElse` fmap Msg.Midi (TChan.readTChan midi_chan)
        `STM.orElse` fmap Msg.Transport (TChan.readTChan player_chan)
        `STM.orElse` fmap Msg.Socket (TChan.readTChan lang_chan)

-- | Accept a connection on the socket, read everything that comes over, then
-- close the socket and place the read data on @output_chan@.
accept_loop socket output_chan = forever $ Exception.bracket
    (Network.accept socket)
    (\(hdl, _, _) -> IO.hClose hdl) $ \(hdl, _host, _port) -> do
        msg <- get_contents_strictly hdl
        STM.atomically $ TChan.writeTChan output_chan msg

get_contents_strictly hdl = do
    eof <- IO.hIsEOF hdl
    if eof then return ""
        else do
            line <- IO.hGetLine hdl
            rest <- get_contents_strictly hdl
            return (line ++ '\n' : rest)

{-
get_contents_strictly :: IO.Handle -> IO String
get_contents_strictly hdl = do
    s <- IO.hGetContents hdl
    return $ length s `seq` s
-}

-- | Everyone always gets these commands.
hardcoded_cmds :: [Cmd.Cmd]
hardcoded_cmds =
    -- Special Cmds that record info about the incoming msgs.
    [ Cmd.cmd_update_ui_state, Cmd.cmd_record_keys, Cmd.cmd_record_active
    , Cmd.cmd_log
    -- Handle special case global msgs.
    , Cmd.cmd_close_window, Play.cmd_transport_msg
    ]

-- | And these special commands that run in IO.
hardcoded_io_cmds transport_info session =
    [ Language.cmd_language session
    , DefaultKeymap.cmd_io_keymap transport_info
    ]

loop :: State.State -> Cmd.State -> MsgReader -> MidiWriter -> Transport.Info
    -> GHC.InterpreterSession -> IO ()
loop ui_state cmd_state get_msg write_midi transport_info session = do
    msg <- get_msg
    -- Apply changes that won't be diffed.  See the 'Cmd.cmd_record_ui_updates'
    -- comment.
    -- TODO: an error implies the UI is out of sync, maybe I should fail more
    -- seriously here?
    let update_cmds = map ($msg) [Cmd.cmd_record_ui_updates]
    (status, ui_state, cmd_state) <- do
        cmd_val <- run_cmds Cmd.run_cmd ui_state cmd_state update_cmds
        handle_cmd_val False write_midi ui_state cmd_val

    -- Certain commands require IO.  Rather than make everything IO,
    -- I hardcode them in a special list that gets run in IO.
    let io_cmds = hardcoded_io_cmds transport_info session
    (status, ui_state, cmd_state) <- maybe_run status write_midi
        ui_state cmd_state Cmd.run io_cmds msg

    let id_cmds = hardcoded_cmds ++ DefaultKeymap.default_cmds cmd_state
    (status, ui_state, cmd_state) <- maybe_run status write_midi
        ui_state cmd_state Cmd.run_cmd id_cmds msg

    case status of
        Cmd.Quit -> return ()
        _ -> loop ui_state cmd_state get_msg write_midi transport_info session

-- | Run the cmds on msg if @status@ is Continue.
maybe_run status write_midi ui_state cmd_state run cmds msg = case status of
    Cmd.Continue -> do
        cmd_val <- run_cmds run ui_state cmd_state (map ($msg) cmds)
        handle_cmd_val True write_midi ui_state cmd_val
    _ -> return (status, ui_state, cmd_state)

-- | Run the given list of Cmds against the Msg, stopping and returning as soon
-- as one doesn't return Continue.  Use the given @run@ function to run the
-- Cmd, since it may be in either IO or Identity.
run_cmds :: (Monad cmd_m, Monad val_m) =>
    Cmd.RunCmd cmd_m val_m -- ^ run the cmd's result monad
    -> State.State -> Cmd.State -- ^ initial states for the cmd
    -> [Cmd.CmdT cmd_m Cmd.Status] -- ^ cmd list
    -> val_m Cmd.CmdVal
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

handle_cmd_val :: Bool -> MidiWriter -> State.State -> Cmd.CmdVal
    -> IO (Cmd.Status, State.State, Cmd.State)
handle_cmd_val do_sync write_midi ui_state1
        (cmd_state, midi, logs, ui_result) = do
    sequence_ [write_midi (Midi.WriteMessage dev Timestamp.immediately msg)
        | (dev, msg) <- midi]
    mapM_ Log.write logs
    (status, ui_state2) <- case ui_result of
        Left err -> do
            Log.error $ "ui error: " ++ show err
            return (Cmd.Done, ui_state1)
        Right (status, ui_state2, cmd_updates) -> do
            when do_sync (sync ui_state1 ui_state2 cmd_updates)
            return (status, ui_state2)
    return (status, ui_state2, cmd_state)

-- Merge updates into a StateT result.
merge_ui_res updates = fmap
    (\(status, ui_state, updates2) -> (status, ui_state, updates ++ updates2))

-- | Sync @state2@ to the UI.
sync :: State.State -> State.State -> [Update.Update] -> IO ()
sync state1 state2 cmd_updates = do
    case Diff.diff state1 state2 of
        Left err -> Log.error $ "diff error: " ++ err
        Right diff_updates -> do
            when (not (null diff_updates) || (not (null cmd_updates))) $
                Log.debug $ "diff_updates: " ++ show diff_updates
                    ++ " cmd_updates: " ++ show cmd_updates
            err <- Sync.sync state2 (diff_updates ++ cmd_updates)
            case err of
                Nothing -> return ()
                Just err -> Log.error $ "syncing updates: " ++ show err


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
