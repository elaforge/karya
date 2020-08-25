-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.NoteEntry_test where
import qualified Util.Log as Log
import Util.Test
import qualified Midi.Midi as Midi
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Internal as Internal
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry

import qualified Perform.Pitch as Pitch
import Global


test_key_to_input :: Test
test_key_to_input = do
    let k = Key.Char
    let f = NoteEntry.key_to_input False

    -- This is assuming dvorak input.
    equal (f 4 True (k '\'')) (Just [kbd_note_on 72 5 0 0])
    equal (f 4 True (k ',')) (Just [kbd_note_on 74 5 1 0])
    equal (f 4 True (k ';')) (Just [kbd_note_on 60 4 0 0])
    equal (f 0 True (k ';')) (Just [kbd_note_on 12 0 0 0])

    equal (f 4 False (k '\'')) (Just [kbd_note_off 72])
    equal (f 4 True (k '[')) Nothing
    equal (f 4 True Key.Backspace) Nothing

    let pressure = NoteEntry.key_to_input True
    equal (pressure 4 True (k '\'')) $ Just
        [ kbd_note_on 72 5 0 0
        , CmdTest.control 72 "breath" InputNote.keyboard_velocity
        ]

test_cmds_with_input :: Test
test_cmds_with_input = do
    let cmd_dummy msg = Log.warn (showt msg) >> return Cmd.Done
    let high_c = '\''
        ctrl_key = CmdTest.make_key_mods [Key.Control] UiMsg.KeyDown
        run cstate cmd = CmdTest.extract id $ CmdTest.run Ui.empty cstate cmd
        input = Msg.InputNote
        -- key passed through to cmd_dummy
        through msg = Right (Just Cmd.Done, [showt msg])
    let f kbd_entry msg = NoteEntry.cmds_with_input kbd_entry Nothing
            [cmd_dummy] msg
    let st = CmdTest.default_cmd_state
            { Cmd.state_edit = (Cmd.state_edit CmdTest.default_cmd_state)
                { Cmd.state_kbd_entry_octave = 4 }
            }

    -- test kbd entry
    equal [run st (f True (CmdTest.key_down c)) | c <- "1'2,"] $
        map (through . input)
            [ kbd_note_on 71 5 0 (-1), kbd_note_on 72 5 0 0
            , kbd_note_on 73 5 0 1, kbd_note_on 74 5 1 0
            ]
    -- With kbd_entry=False, it stays as a keydown.
    equal (run st (f False (CmdTest.key_down high_c))) $
        through $ CmdTest.key_down high_c

    -- abort when a modifier is down
    let ckey = ctrl_key (Key.Char high_c)
    equal (run (with_key (ctrl_key Key.ControlL)) (f True ckey)) (through ckey)

    equal (run st (f True (CmdTest.key_down ';')))
        (through $ input (kbd_note_on 60 4 0 0))
    equal (run st (f True (CmdTest.key_up ';')))
        (through $ input (kbd_note_off 60))

    let oct5 = st { Cmd.state_edit = (Cmd.state_edit st)
            { Cmd.state_kbd_entry_octave = 5 } }
    equal (run oct5 (f True (CmdTest.key_down high_c)))
        (through $ input (kbd_note_on 84 6 0 0))
    -- with kbd_entry off, keys don't get mapped
    equal (run st (f False (CmdTest.key_down high_c)))
        (through (CmdTest.key_down high_c))

    -- test midi_entry (details tested in InputNote_test)
    equal (run st (f True (CmdTest.make_midi (Midi.NoteOn 25 127))))
        (through $ input (CmdTest.note_on_nn 25))
    -- Doesn't require kbd_entry.
    equal (run st (f False (CmdTest.make_midi (Midi.NoteOn 25 127))))
        (through $ input (CmdTest.note_on_nn 25))
    equal (run st (f False (CmdTest.make_midi (Midi.NoteOff 25 127))))
        (through $ input (CmdTest.note_off 25))

with_key :: Msg.Msg -> Cmd.State
with_key key = CmdTest.result_cmd_state $
    CmdTest.run Ui.empty CmdTest.default_cmd_state
        (Internal.cmd_record_keys key)

kbd_note_on :: Int -> Pitch.Octave -> Pitch.PitchClass -> Pitch.Accidentals
    -> InputNote.Input
kbd_note_on note_id oct pc accs =
    InputNote.NoteOn (InputNote.NoteId note_id)
        (Pitch.Input Pitch.AsciiKbd (CmdTest.pitch oct pc accs) 0)
        InputNote.keyboard_velocity

kbd_note_off :: Int -> InputNote.GenericInput a
kbd_note_off note_id = InputNote.NoteOff (InputNote.NoteId note_id)
    InputNote.keyboard_velocity
