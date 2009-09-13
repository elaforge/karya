module Cmd.KbdEntry_test where
import Util.Test

import qualified Midi.Midi as Midi
import qualified Util.Log as Log

import qualified Ui.Key as Key
import qualified Ui.State as State

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.KbdEntry as KbdEntry
import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd

import qualified Derive.Scale.Twelve as Twelve

with_key :: Msg.Msg -> Cmd.State
with_key key =
    extract $ CmdTest.run State.empty CmdTest.cmd_state
        (Cmd.cmd_record_keys key)
    where
    extract (Right (_, _, cmd_state, _)) = cmd_state
    extract val = error $ "with_keys: " ++ show val

test_with_note = do
    let cmd_dummy msg = Log.warn (show msg) >> return Cmd.Done
    let f kbd_entry msg = KbdEntry.with_note kbd_entry cmd_dummy msg
        key = CmdTest.key_down ','
        note_key = CmdTest.input (5, 2)
        run cstate cmd = extract_logs $ CmdTest.run State.empty cstate cmd
        -- key passed through to cmd_dummy
        through msg = Right (Just [show msg])

    -- abort when a modifier is down
    equal (run (with_key (CmdTest.make_key True Key.ControlL)) (f True key))
        (through key)

    let st = Cmd.empty_state
    equal (run st (f True key)) (through note_key)
    equal (run (st { Cmd.state_kbd_entry_octave = 5 }) (f True key))
        (through (CmdTest.input (6, 2)))
    equal (run st (f False key)) (through key)
    equal (run st (f True (CmdTest.key_up ',')))
        (Right (Just [show (CmdTest.key_up ',')]))

    equal (run st (f True (CmdTest.make_midi (Midi.NoteOn 25 20))))
        (Right (Just ["InputKey (InputKey (2,1))"]))
    equal (run st (f True (CmdTest.make_midi (Midi.NoteOff 25 20))))
        (through (CmdTest.make_midi (Midi.NoteOff 25 20)))

extract_logs :: Either State.StateError (Maybe a, b, c, [Log.Msg])
    -> Either String (Maybe [String])
extract_logs (Right (val, _cmd_state, _ui_state, logs)) = case val of
    Nothing -> Right Nothing
    Just _ -> Right (Just (map Log.msg_text logs))
extract_logs (Left err) = Left (show err)

test_with_midi = do
    let cmd_dummy (Msg.Midi rmsg) = Log.warn (show (Midi.rmsg_msg rmsg))
            >> return Cmd.Done
        cmd_dummy msg = error (show msg)
    let f = KbdEntry.with_midi Twelve.scale_id cmd_dummy
        key = CmdTest.key_down ','
        run cstate cmd = extract_logs $ CmdTest.run State.empty cstate cmd
    let st = Cmd.empty_state

    equal (run st (f key))
        (Right (Just ["ChannelMessage 0 (NoteOn 62 100)"]))
    equal (run st (f (CmdTest.make_key False (Key.KeyChar ','))))
        (Right (Just ["ChannelMessage 0 (NoteOff 62 100)"]))
    equal (run (st {Cmd.state_kbd_entry_octave = 5 }) (f key))
        (Right (Just ["ChannelMessage 0 (NoteOn 74 100)"]))

    let st = with_key (CmdTest.make_key True Key.ControlL)
    equal (run st (f key)) (Right Nothing)

test_midi_from_kbd = do
    let f octave down key = extract $ KbdEntry.midi_from_kbd
            Twelve.scale octave (CmdTest.make_key down key)
        extract (Just (Msg.Midi rmsg)) = Just (Midi.rmsg_msg rmsg)
        extract _ = Nothing
        mkmsg = Midi.ChannelMessage 0
    equal (f 0 True (Key.KeyChar ';')) $
        Just (mkmsg (Midi.NoteOn 0 100))
    equal (f 0 False (Key.KeyChar ';')) $
        Just (mkmsg (Midi.NoteOff 0 100))
    equal (f 1 True (Key.KeyChar ';')) $
        Just (mkmsg (Midi.NoteOn 12 100))
    equal (f 0 True (Key.KeyChar '\'')) $
        Just (mkmsg (Midi.NoteOn 12 100))

    equal (f 0 True (Key.KeyChar '\'')) $
        Just (mkmsg (Midi.NoteOn 12 100))
    equal (f 0 True (Key.KeyChar '1')) $
        Just (mkmsg (Midi.NoteOn 11 100)) -- 23

    equal (f 0 True Key.Backspace) Nothing
    equal (f 0 True (Key.KeyChar '[')) Nothing
