module Cmd.Responder_profile where
import qualified Data.Map as Map

import Util.Test
import qualified Util.Log as Log

import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.TimeStep as TimeStep


profile_null_cmd = do
    -- Test a msg that matches no cmds so I can see how much garbage it
    -- produces by itself.
    let states = ResponderTest.mkstates [(">i", [(0, 0, "")])]
    let key = keypress Key.ShiftL
    let keys = take (10*1024) (cycle key)
    (updates, midi, states) <- print_timer $ ResponderTest.respond states keys
    pprint (length updates)

profile_selection = do
    Log.configure (\st -> st { Log.state_log_level = Log.Warn })
    let (ui_state, cmd_state) = ResponderTest.mkstates [(">i", [(0, 0, "")])]
    let ui_state2 = ui_state { State.state_rulers =
            Map.insert (UiTest.rid "b1.r0") (UiTest.mkruler 256 1)
                (State.state_rulers ui_state) }
    let cmd_state2 = cmd_state
            { Cmd.state_step = TimeStep.AbsoluteMark TimeStep.AllMarklists
                (TimeStep.MatchRank 3 0)
            }
    let states = (ui_state2, cmd_state2)
    let one_cycle = take (256*2) (cycle (keypress Key.Down))
            ++ take (256*2) (cycle (keypress Key.Up))
    let keys = take (10*1024) (cycle one_cycle)
    (updates, midi, states) <- print_timer $ ResponderTest.respond states keys
    pprint (length updates)

keypress k = [CmdTest.make_key True k, CmdTest.make_key False k]


-- reintegrate these profiles
{-

-- test_thru = do
    res <- with_inst [CmdTest.make_midi $ Midi.NoteOn 10 20]
    pprint res
    -- TODO send a lot thru and do timing

-- test_thru_timing = do
    let extract (_, midi) = midi
        many_msgs = [CmdTest.make_midi msg
            | n <- cycle [0..127], msg <- [Midi.NoteOn n 60, Midi.NoteOff n 60]]
        msg_count = 100 -- increase this for real profiling
        msgs = take msg_count many_msgs
    print (length msgs)
    -- This is awfully spammy otherwise.
    log_state <- Log.configure $ \st -> st { Log.state_log_level = Log.Warn }
    secs <- time_op $ do
        midi <- fmap extract $ with_inst msgs
        putStrLn $ "midi back: " ++ show (length midi)
    print (secs, secs / fromIntegral msg_count)
    Log.configure (const log_state)

time_op :: IO a -> IO Double
time_op op = do
    start <- CPUTime.getCPUTime
    op
    end <- CPUTime.getCPUTime
    return (fromIntegral (end-start) / (10**12))

with_inst msgs = do
    let (_, ustate) = UiTest.run_mkview [(">i0", [])]
    let cstate = CmdTest.default_cmd_state
    let (ustate2, cstate2) = CmdTest.set_insts ["i0"] ustate cstate
    run_msgs ustate2 cstate2 msgs

-}
