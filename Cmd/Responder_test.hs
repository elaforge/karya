module Cmd.Responder_test where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Map as Map
import qualified System.IO as IO
import qualified System.CPUTime as CPUTime

import qualified Midi.CoreMidi as CoreMidi
import qualified Midi.Midi as Midi

import qualified Util.Log as Log
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
    res <- with_inst [CmdTest.make_midi $ Midi.NoteOn 10 20]
    pprint res
    -- TODO send a lot thru and do timing

test_thru_timing = do
    let extract (_, midi) = midi
        many_msgs = [CmdTest.make_midi msg
            | n <- cycle [0..127], msg <- [Midi.NoteOn n 60, Midi.NoteOff n 60]]
        msg_count = 100 -- increase this for real profiling
        msgs = take msg_count many_msgs
    print (length msgs)
    -- This is awfully spammy otherwise.
    log_state <- Log.initialize Nothing Log.Warn
    secs <- time_op $ do
        midi <- fmap extract $ with_inst msgs
        putStrLn $ "midi back: " ++ show (length midi)
    print (secs, secs / fromIntegral msg_count)
    Log.swap_state log_state

time_op :: IO a -> IO Double
time_op op = do
    start <- CPUTime.getCPUTime
    op
    end <- CPUTime.getCPUTime
    return (fromIntegral (end-start) / (10**12))

{-
-- test_keydown = do
    let (_, ustate) = UiTest.run_mkview []
    let cstate = CmdTest.cmd_state
    let extract ([((res, cstate), _)], _) =
            (res2, Map.keys (Cmd.state_keys_down cstate))
            where
            res2 = case res of
                Left err -> Just (show err)
                _ -> Nothing
    let run msg = fmap extract (run_msgs ustate cstate [msg])
    -- Make sure keydown works even when the cmd throws.
    -- This is brittle because it relies on the definition of ' '.
    io_equal (run (CmdTest.make_key True (Key.KeyChar ' ')))
        (Just "StateError: player thread not running",
            [Cmd.KeyMod (Key.KeyChar ' ')])
-}

with_inst msgs = do
    let (_, ustate) = UiTest.run_mkview [(">i0", [])]
    let cstate = CmdTest.cmd_state
    let (ustate2, cstate2) = CmdTest.set_insts ["i0"] ustate cstate
    run_msgs ustate2 cstate2 msgs

dummy_transport :: IO Transport.Info
dummy_transport = do
    chan <- TChan.newTChanIO
    return $ Transport.Info chan (const (return ())) (return ()) CoreMidi.now

run_msgs :: State.State -> Cmd.State -> [Msg.Msg]
    -> IO ([Cmd.Status], [Midi.WriteMessage])
run_msgs ustate cstate msgs = do
    midi <- MVar.newMVar []
    let midi_writer wmsg = MVar.modifyMVar_ midi (\ms -> return (wmsg:ms))
    interpreter_chan <- Chan.newChan
    transport_info <- dummy_transport
    let rstate = Responder.ResponderState StaticConfig.empty_config
            ustate cstate (error "state_msg_reader unused") midi_writer
            transport_info interpreter_chan
    statuses <- thread_states rstate msgs
    midi_msgs <- MVar.takeMVar midi
    return (statuses, midi_msgs)

thread_states rstate (msg:msgs) = do
    ((res, cmd_state), updates) <- Responder.run_responder
        (Responder.run_cmds rstate msg)
    rstate <- return $ rstate { Responder.state_cmd = cmd_state }
    (status, rstate) <- case res of
        Left err -> do
            putStrLn $ "responder: " ++ show err
            return (Cmd.Continue, rstate)
        Right (status, ui_from, ui_to) -> do
            -- Log.timer "syncing"
            -- cmd_state <- return $ fix_cmd_state ui_to cmd_state
            -- (updates, ui_state, cmd_state) <-
            --     ResponderSync.sync ui_from ui_to cmd_state updates
            -- cmd_state <- record_history updates ui_from cmd_state
            return (status,
                rstate { Responder.state_cmd = cmd_state,
                    Responder.state_ui = ui_to })
    rest <- thread_states rstate msgs
    return $ status :  rest
thread_states _ [] = return []
