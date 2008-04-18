module Cmd.Responder where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Monad.Identity as Identity
import qualified Util.Log as Log

import qualified Ui.State
import qualified Ui.Sync as Sync
import qualified Ui.Diff as Diff
import qualified Ui.Update as Update
import qualified Ui.UiMsg as UiMsg
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg


type MidiWriter = Midi.CompleteMessage -> IO ()
type MsgReader = IO Msg.Msg

responder ui_state = loop ui_state Cmd.initial_state

loop :: Ui.State.State -> Cmd.State -> MsgReader -> MidiWriter -> IO ()
loop ui_state1 cmd_state1 get_msg write_midi = do
    msg <- get_msg
    -- later this is hardcoded list merged with Block and Track mappings
    let cmd_stack = [Cmd.cmd_record_keys, Cmd.cmd_log, Cmd.cmd_quit]
    let (cmd_status, cmd_state2, midi, log_msgs, ui_result) =
            do_cmds ui_state1 cmd_state1 cmd_stack msg
    sequence_ [write_midi (dev, 0, msg) | (dev, msg) <- midi]
    mapM_ Log.write log_msgs
    ui_state2 <- case ui_result of
        Left err -> do
            Log.error $ "ui error: " ++ show err
            return ui_state1
        Right (ui_state2, cmd_updates) -> do
            sync ui_state1 ui_state2 cmd_updates
            return ui_state2

    case cmd_status of
        Cmd.Quit -> return ()
        _ -> loop ui_state2 cmd_state2 get_msg write_midi

-- | Create the MsgReader to pass to 'loop'.
create_msg_reader :: TChan.TChan UiMsg.UiMsg
    -> TChan.TChan Midi.CompleteMessage -> MsgReader
create_msg_reader ui_chan midi_chan = STM.atomically $
    fmap Msg.Ui (TChan.readTChan ui_chan)
    `STM.orElse` fmap Msg.Midi (TChan.readTChan midi_chan)

-- | Run each of @cmds@ in order, stopping as soon as one doesn't return
-- Continue.  Their various return lists are concatenated and returned.
do_cmds :: Ui.State.State -> Cmd.State -> [Cmd.Cmd] -> Msg.Msg -> Cmd.CmdVal
do_cmds ui_state cmd_state cmds msg = Cmd.run_cmd ui_state cmd_state $ do
    sequence_while (== Cmd.Continue) Cmd.Done (map ($ msg) cmds)

-- | Sync @state2@ to the UI.
sync :: Ui.State.State -> Ui.State.State -> [Update.Update] -> IO ()
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
sequence_while pred zero [] = return zero
sequence_while pred zero (op:ops) = do
    val <- op
    if pred val
        then sequence_while pred zero ops
        else return val
