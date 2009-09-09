module Cmd.Responder_test where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Map as Map
import qualified System.IO as IO

import qualified Midi.CoreMidi as CoreMidi
import qualified Midi.Midi as Midi

import Util.Test

import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Update as Update
import qualified Ui.UiTest as UiTest

import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Responder as Responder
import qualified Cmd.CmdTest as CmdTest

import qualified Perform.Transport as Transport

import qualified App.StaticConfig as StaticConfig


test_thru = do
    let extract ((res, _), _updates, midi) = case res of
            Left (State.StateError err) -> Left (show err)
            Right (status, _, _) -> Right (status, midi)
    res <- fmap extract $ with_inst $ CmdTest.make_midi $ Midi.NoteOn 10 20
    pprint res
    -- TODO send a lot thru and do timing

test_keydown = do
    let (_, ustate) = UiTest.run_mkview []
    let cstate = CmdTest.cmd_state
    let extract ((res, cstate), _, _) =
            (res2, Map.keys (Cmd.state_keys_down cstate))
            where
            res2 = case res of
                Left err -> Just (show err)
                _ -> Nothing
    let run msg = fmap extract (run_msg ustate cstate msg)
    -- Make sure keydown works even when the cmd throws.
    -- This is brittle because it relies on the definition of ' '.
    io_equal (run (CmdTest.make_key True (Key.KeyChar ' ')))
        (Just "StateError: player thread not running",
            [Cmd.KeyMod (Key.KeyChar ' ')])

with_inst msg = do
    let (_, ustate) = UiTest.run_mkview [(">i0", [])]
    let cstate = CmdTest.cmd_state
    let (ustate2, cstate2) = CmdTest.set_insts ["i0"] ustate cstate
    run_msg ustate2 cstate2 msg

dummy_transport :: IO Transport.Info
dummy_transport = do
    chan <- TChan.newTChanIO
    return $ Transport.Info chan (const (return ())) (return ()) CoreMidi.now

run_msg :: State.State -> Cmd.State -> Msg.Msg
    -> IO (Responder.RType, [Update.Update], [Midi.WriteMessage])
run_msg ustate cstate msg = do
    midi <- MVar.newMVar []
    let midi_writer wmsg = MVar.modifyMVar_ midi (\ms -> return (wmsg:ms))
    interpreter_chan <- Chan.newChan
    transport_info <- dummy_transport
    let rstate = Responder.ResponderState StaticConfig.empty_config
            ustate cstate (return msg) midi_writer transport_info
            interpreter_chan
    (res, updates) <- Responder.run_responder (Responder.run_cmds rstate msg)
    midi_msgs <- MVar.takeMVar midi
    return (res, updates, midi_msgs)
