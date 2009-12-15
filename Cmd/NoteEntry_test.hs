module Cmd.NoteEntry_test where
import Util.Test

import qualified Midi.Midi as Midi
import qualified Util.Log as Log

import qualified Ui.Key as Key
import qualified Ui.State as State

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.NoteEntry as NoteEntry
import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd


test_key_to_input = do
    let k = Key.KeyChar
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
        run cstate cmd = extract_logs $ CmdTest.run State.empty cstate cmd
        input = Msg.InputNote
        -- key passed through to cmd_dummy
        through msg = Right (Just Cmd.Done, [show msg])
    let f kbd_entry msg = NoteEntry.cmds_with_note kbd_entry [cmd_dummy] msg

    -- test kbd entry

    -- abort when a modifier is down
    equal (run (with_key (CmdTest.make_key True Key.ControlL)) (f True key))
        (through key)

    let st = Cmd.empty_state
    equal (run st (f True key))
        (through $ input (CmdTest.note_on 62 62 100))
    equal (run st (f True (CmdTest.key_up ',')))
        (through $ input (CmdTest.note_off 62 100))

    equal (run (st { Cmd.state_kbd_entry_octave = 5 }) (f True key))
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
with_key key =
    extract $ CmdTest.run State.empty CmdTest.cmd_state
        (Cmd.cmd_record_keys key)
    where
    extract (Right (_, _, cmd_state, _)) = cmd_state
    extract val = error $ "with_keys: " ++ show val

extract_logs :: Either State.StateError (Maybe Cmd.Status, b, c, [Log.Msg])
    -> Either String (Maybe Cmd.Status, [String])
extract_logs (Right (val, _cmd_state, _ui_state, logs)) =
    Right (val, map Log.msg_string logs)
extract_logs (Left err) = Left (show err)
