module Cmd.NoteEntry_test where
import qualified Util.Log as Log
import Util.Test
import qualified Midi.Midi as Midi
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Internal as Internal
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry


test_key_to_input = do
    let k = Key.Char
    let f = NoteEntry.key_to_input
    equal (f 4 True (k '\'')) (Just (Just (CmdTest.note_on 60 60 100)))
    equal (f 4 True (k ',')) (Just (Just (CmdTest.note_on 62 62 100)))
    equal (f 4 True (k ';')) (Just (Just (CmdTest.note_on 48 48 100)))
    equal (f 0 True (k ';')) (Just (Just (CmdTest.note_on 0 0 100)))

    equal (f 4 False (k '\'')) (Just (Just (CmdTest.note_off 60 100)))
    equal (f 4 True (k 'a')) (Just Nothing)
    equal (f 4 True (k '[')) Nothing
    equal (f 4 True Key.Backspace) Nothing

test_cmds_with_note = do
    let cmd_dummy msg = Log.warn (show msg) >> return Cmd.Done
    let key = CmdTest.key_down ','
        ctrl_key = CmdTest.make_key_mods [Key.Control] UiMsg.KeyDown
        run cstate cmd = CmdTest.extract id $
            CmdTest.run State.empty cstate cmd
        input = Msg.InputNote
        -- key passed through to cmd_dummy
        through msg = Right (Just Cmd.Done, [show msg])
    let f kbd_entry msg = NoteEntry.cmds_with_note kbd_entry [cmd_dummy] msg

    -- test kbd entry

    -- abort when a modifier is down
    let ckey = ctrl_key (Key.Char ',')
    equal (run (with_key (ctrl_key Key.ControlL)) (f True ckey))
        (through ckey)

    let st = Cmd.empty_state
    equal (run st (f True key))
        (through $ input (CmdTest.note_on 62 62 100))
    equal (run st (f True (CmdTest.key_up ',')))
        (through $ input (CmdTest.note_off 62 100))

    let oct5 = st { Cmd.state_edit = (Cmd.state_edit st)
            { Cmd.state_kbd_entry_octave = 5 } }
    equal (run oct5 (f True key))
        (through $ input (CmdTest.note_on 74 74 100))
    -- cmd not called, and further cmds skipped
    equal (run st (f True (CmdTest.key_down 'a')))
        (Right (Just Cmd.Done, []))
    -- with kbd_entry off, keys don't get mapped
    equal (run st (f False key))
        (through key)

    -- test midi_entry (details tested in InputNote_test)
    equal (run st (f True (CmdTest.make_midi (Midi.NoteOn 25 20))))
        (through $ input (CmdTest.note_on 25 25 20))
    equal (run st (f True (CmdTest.make_midi (Midi.NoteOff 25 20))))
        (through $ input (CmdTest.note_off 25 20))

with_key :: Msg.Msg -> Cmd.State
with_key key = CmdTest.result_cmd_state $
    CmdTest.run State.empty CmdTest.default_cmd_state
        (Internal.cmd_record_keys key)
