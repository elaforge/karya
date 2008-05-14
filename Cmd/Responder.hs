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
import qualified Derive.Player as Player
import qualified Perform.Timestamp as Timestamp

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.DefaultKeymap as DefaultKeymap
import qualified Cmd.Play as Play


type MidiWriter = Midi.WriteMessage -> IO ()
type MsgReader = IO Msg.Msg

responder :: MsgReader -> MidiWriter -> IO Timestamp.Timestamp -> Player.Chan
    -> Cmd.CmdM -> IO ()
responder get_msg write_midi get_ts player_chan setup_cmd = do
    Log.notice "starting responder"
    let ui_state = State.empty
    (_, ui_state, cmd_state) <- handle_cmd_result True write_midi ui_state
        (Cmd.run_cmd ui_state Cmd.empty_state setup_cmd)
    loop ui_state cmd_state get_msg write_midi
        (Player.Info player_chan write_midi get_ts)

-- | Create the MsgReader to pass to 'responder'.
create_msg_reader :: TChan.TChan UiMsg.UiMsg
    -> TChan.TChan Midi.ReadMessage -> Player.Chan -> MsgReader
create_msg_reader ui_chan midi_chan player_chan = STM.atomically $
    fmap Msg.Ui (TChan.readTChan ui_chan)
    `STM.orElse` fmap Msg.Midi (TChan.readTChan midi_chan)
    `STM.orElse` fmap Msg.Player (TChan.readTChan player_chan)

-- | Everyone always gets these commands.
hardcoded_cmds :: [Cmd.Cmd]
hardcoded_cmds =
    -- Special Cmds that record info about the incoming msgs.
    [ Cmd.cmd_update_ui_state, Cmd.cmd_record_keys, Cmd.cmd_record_active
    , Cmd.cmd_log
    -- Handle special case global msgs.
    , Cmd.cmd_close_window, Play.cmd_player_msg
    ]

loop :: State.State -> Cmd.State -> MsgReader -> MidiWriter -> Player.Info
    -> IO ()
loop ui_state cmd_state get_msg write_midi player_info = do
    msg <- get_msg
    -- Apply changes that won't be diffed.  See the 'Cmd.cmd_record_ui_updates'
    -- comment.
    -- TODO: an error implies the UI is out of sync, maybe I should fail more
    -- seriously here?
    (status, ui_state, cmd_state) <- handle_cmd_result
        False write_midi ui_state
        (Cmd.run_cmd ui_state cmd_state (Cmd.cmd_record_ui_updates msg))

    -- TODO: prepend cmds for the active Block and Track to cmds.
    let cmds = hardcoded_cmds ++ DefaultKeymap.default_cmds cmd_state
    (status, ui_state, cmd_state) <- case status of
        Cmd.Continue -> handle_cmd_result True write_midi
            ui_state (run_cmds ui_state cmd_state cmds msg)
        _ -> return (status, ui_state, cmd_state)

    (ui_state, cmd_state) <- run_io_cmd write_midi ui_state cmd_state
        (DefaultKeymap.cmd_io_keymap player_info msg)

    case status of
        Cmd.Quit -> return ()
        _ -> loop ui_state cmd_state get_msg write_midi player_info

run_io_cmd write_midi ui_state cmd_state cmd = do
    result <- Cmd.run ui_state cmd_state cmd
    (_status, ui_state, cmd_state) <-
        handle_cmd_result True write_midi ui_state result
    return (ui_state, cmd_state)

handle_cmd_result :: Bool -> MidiWriter -> State.State -> Cmd.CmdVal
    -> IO (Cmd.Status, State.State, Cmd.State)
handle_cmd_result do_sync write_midi ui_state1
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

run_cmds :: State.State -> Cmd.State -> [Cmd.Cmd] -> Msg.Msg -> Cmd.CmdVal
run_cmds ui_state cmd_state [] _msg =
    (cmd_state, [], [], Right (Cmd.Done, ui_state, []))
run_cmds ui_state cmd_state (cmd:cmds) msg =
    let (cmd_state1, midi1, logs1, ui_res1) =
            Cmd.run_cmd ui_state cmd_state (cmd msg)
    in case ui_res1 of
        Right (Cmd.Continue, ui_state1, updates1) ->
            let (cmd_state2, midi2, logs2, ui_res2) =
                    run_cmds ui_state1 cmd_state1 cmds msg
            in (cmd_state2, midi1 ++ midi2, logs1 ++ logs2,
                merge_ui_res updates1 ui_res2)
        Left err -> (cmd_state1, midi1, logs1, Left err)
        -- It's Quit or Done, so return as-is.
        _ -> (cmd_state1, midi1, logs1, ui_res1)

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
