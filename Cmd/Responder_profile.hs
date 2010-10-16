module Cmd.Responder_profile where
import qualified Data.Map as Map

import qualified Midi.Midi as Midi

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
    (_, cpu) <- timer $ ResponderTest.respond states keys
    printf "%.2f sec, %.4f sec per cmd\n" cpu (cpu / (10*1024))

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
    (_, cpu) <- timer $ ResponderTest.respond states keys
    printf "%.2f sec, %.4f sec per cmd\n" cpu (cpu / (10*1024))

keypress k = [CmdTest.make_key True k, CmdTest.make_key False k]

profile_thru = do
    let (ui_state, cmd_state) = ResponderTest.mkstates [(">i", [(0, 0, "")])]
    let states = CmdTest.set_insts ["i"] ui_state cmd_state
    let ncmds = 10 * 1024
    let key = [CmdTest.make_midi (Midi.NoteOn 60 20),
            CmdTest.make_midi (Midi.NoteOff 60 20)]
        keys = take ncmds (cycle key)
    ((_, midi, _), cpu) <- timer $ ResponderTest.respond states keys
    printf "%.2f sec, %.4f sec per cmd\n" cpu (cpu / fromIntegral ncmds)
    print (length midi)
