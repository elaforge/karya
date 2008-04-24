module Cmd.Responder where

import Control.Monad
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Util.Log as Log

import qualified Ui.State as State
import qualified Ui.Sync as Sync
import qualified Ui.Diff as Diff
import qualified Ui.Update as Update
import qualified Ui.UiMsg as UiMsg
import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.DefaultKeymap as DefaultKeymap


type MidiWriter = Midi.CompleteMessage -> IO ()
type MsgReader = IO Msg.Msg

responder :: MsgReader -> MidiWriter -> Cmd.CmdM -> IO ()
responder get_msg write_midi setup_cmd = do
    Log.notice "starting responder"
    let ui_state = State.empty
    (_, ui_state, cmd_state) <- handle_cmd_result True write_midi ui_state
        (Cmd.run_cmd ui_state Cmd.empty_state setup_cmd)
    loop ui_state cmd_state get_msg write_midi

-- Hardcoded cmds.
cmd_stack = [Cmd.cmd_log, Cmd.cmd_quit, Cmd.cmd_close_window,
    Cmd.cmd_record_keys, Cmd.cmd_record_active, DefaultKeymap.default_keymap]

loop :: State.State -> Cmd.State -> MsgReader -> MidiWriter -> IO ()
loop ui_state1 cmd_state1 get_msg write_midi = do
    msg <- get_msg
    -- Apply changes that won't be diffed.  See 'Cmd.cmd_record_ui_updates'
    -- comment.
    -- TODO: an error implies the UI is out of sync, maybe I should fail more
    -- seriously here?
    (status, ui_state1, cmd_state1) <- handle_cmd_result
        False write_midi ui_state1
        (Cmd.run_cmd ui_state1 cmd_state1 (Cmd.cmd_record_ui_updates msg))

    -- TODO: prepend cmds for the active Block and Track to cmd_stack.
    (status, ui_state2, cmd_state2) <- case status of
        Cmd.Continue -> handle_cmd_result True write_midi
            ui_state1 (run_cmds ui_state1 cmd_state1 cmd_stack msg)
        _ -> return (status, ui_state1, cmd_state1)

    case status of
        Cmd.Quit -> return ()
        _ -> loop ui_state2 cmd_state2 get_msg write_midi

handle_cmd_result :: Bool -> MidiWriter -> State.State -> Cmd.CmdVal
    -> IO (Cmd.Status, State.State, Cmd.State)
handle_cmd_result do_sync write_midi ui_state1
    (cmd_state, midi, logs, ui_result) = do
        sequence_ [write_midi (dev, Midi.immediately, msg) | (dev, msg) <- midi]
        mapM_ Log.write logs
        (status, ui_state2) <- case ui_result of
            Left err -> do
                Log.error $ "ui error: " ++ show err
                return (Cmd.Done, ui_state1)
            Right (status, ui_state2, cmd_updates) -> do
                when do_sync (sync ui_state1 ui_state2 cmd_updates)
                return (status, ui_state2)
        return (status, ui_state2, cmd_state)

-- | Create the MsgReader to pass to 'loop'.
create_msg_reader :: TChan.TChan UiMsg.UiMsg
    -> TChan.TChan Midi.CompleteMessage -> MsgReader
create_msg_reader ui_chan midi_chan = STM.atomically $
    fmap Msg.Ui (TChan.readTChan ui_chan)
    `STM.orElse` fmap Msg.Midi (TChan.readTChan midi_chan)

run_cmds :: State.State -> Cmd.State -> [Cmd.Cmd] -> Msg.Msg -> Cmd.CmdVal
run_cmds ui_state cmd_state [] _msg =
    (cmd_state, [], [], Right (Cmd.Done, ui_state, []))
run_cmds ui_state cmd_state (cmd:cmds) msg =
    let (cmd_state1, midi1, logs1, ui_res1) =
            Cmd.run_cmd ui_state cmd_state (cmd msg)
    in case ui_res1 of
        Left err -> (cmd_state1, midi1, logs1, Left err)
        Right (Cmd.Continue, ui_state2, updates1) ->
            let (cmd_state2, midi2, logs2, ui_res2) =
                    run_cmds ui_state2 cmd_state2 cmds msg
            in (cmd_state2, midi1 ++ midi2, logs1 ++ logs2, ui_res2)
        -- It's Quit or Done, so return as-is.
        _ -> (cmd_state1, midi1, logs1, ui_res1)

-- | Sync @state2@ to the UI.
sync :: State.State -> State.State -> [Update.Update] -> IO ()
sync state1 state2 cmd_updates = do
    case Diff.diff state1 state2 of
        Left err -> Log.error $ "diff error: " ++ show err
        Right diff_updates -> do
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
