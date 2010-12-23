module Cmd.Responder_profile where
import qualified Data.Map as Map

import qualified Midi.Midi as Midi

import Util.Test
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Derive_profile as Derive_profile


profile_edits_middle = do
    -- Test editing a large score in the middle as a real user would.
    Log.configure $ \st -> st { Log.state_log_level = Log.Warn }
    let edit_block = UiTest.bid "b1.5.0"
    let (view_id, ui_state) = UiTest.run State.empty $ do
            ResponderTest.set_midi_config
            Derive_profile.make_nested_controls "b1" 15 3 60
            view_id <- Create.view edit_block
            UiTest.select_point view_id 1 0.0
            return view_id
    let cmd_state = modify_edit_state (ResponderTest.mk_cmd_state view_id)
            (\st -> st { Cmd.state_edit_mode = Cmd.ValEdit })
    -- pprint (UiTest.dump_block ui_state edit_block)

    let wait = [(CmdTest.make_key True Key.ShiftL, 0.1),
            (CmdTest.make_key False Key.ShiftL, 4)]
        alter_note = [(CmdTest.m_note_on 0 64 127, 1),
            (CmdTest.m_note_off 0 127, 1)]
        keys = concat $ take 4 $ repeat alter_note
    (updates, _mthru, states) <-
        ResponderTest.respond_delay (ui_state, cmd_state) (wait ++ keys)
    return ()

profile_null_cmd = do
    -- Test a msg that matches no cmds so I can see how much garbage it
    -- produces by itself.
    let states = ResponderTest.mkstates [(">i", [(0, 0, "")])]
    let key = keypress Key.ShiftL
    let keys = take (10*1024) (cycle key)
    (_, cpu) <- timer $ ResponderTest.respond states keys
    printf "%.2f sec, %.4f sec per cmd\n" cpu (cpu / (10*1024))

profile_selection = do
    Log.configure $ \st -> st { Log.state_log_level = Log.Warn }
    let (ui_state, cmd_state) = ResponderTest.mkstates [(">i", [(0, 0, "")])]
    let ui_state2 = ui_state { State.state_rulers =
            Map.insert (UiTest.rid "b1.r0") (UiTest.mkruler 256 1)
                (State.state_rulers ui_state) }
    let cmd_state2 = modify_edit_state cmd_state $ \st -> st
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

modify_edit_state st f = st { Cmd.state_edit = f (Cmd.state_edit st) }
