-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.NoteEntry_test where
import qualified Util.Log as Log
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Internal as Internal
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteEntry as NoteEntry

import qualified Midi.Midi as Midi
import qualified Perform.Pitch as Pitch
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg

import           Global
import           Util.Test


test_key_to_input :: Test
test_key_to_input = do
    let k = Key.Char
    let f = NoteEntry.keyToInput False

    -- This is assuming dvorak input.
    equal (f 4 True (k '\'')) (Just [kbdNoteOn 72 5 0 0])
    equal (f 4 True (k ',')) (Just [kbdNoteOn 74 5 1 0])
    equal (f 4 True (k ';')) (Just [kbdNoteOn 60 4 0 0])
    equal (f 0 True (k ';')) (Just [kbdNoteOn 12 0 0 0])

    equal (f 4 False (k '\'')) (Just [kbdNoteOff 72])
    equal (f 4 True (k '[')) Nothing
    equal (f 4 True Key.Backspace) Nothing

    let pressure = NoteEntry.keyToInput True
    equal (pressure 4 True (k '\'')) $ Just
        [ kbdNoteOn 72 5 0 0
        , CmdTest.control 72 "breath" InputNote.keyboardVelocity
        ]

test_cmds_with_input :: Test
test_cmds_with_input = do
    let cmdDummy msg = Log.warn (showt msg) >> pure Cmd.Done
    let highC = '\''
        ctrlKey = CmdTest.makeKeyMods [Key.Control] UiMsg.KeyDown
        run cstate cmd = CmdTest.extract id $ CmdTest.run Ui.empty cstate cmd
        input = Msg.InputNote
        -- key passed through to cmd_dummy
        through msg = Right (Just Cmd.Done, [showt msg])
    let f kbdEntry msg = NoteEntry.cmdsWithInput kbdEntry Nothing
            [cmdDummy] msg
    let st = CmdTest.defaultCmdState
            { Cmd.stateEdit = (Cmd.stateEdit CmdTest.defaultCmdState)
                { Cmd.stateKbdEntryOctave = 4 }
            }

    -- test kbd entry
    equal [run st (f True (CmdTest.keyDown c)) | c <- "1'2,"] $
        map (through . input)
            [ kbdNoteOn 71 5 0 (-1), kbdNoteOn 72 5 0 0
            , kbdNoteOn 73 5 0 1, kbdNoteOn 74 5 1 0
            ]
    -- With kbd_entry=False, it stays as a keydown.
    equal (run st (f False (CmdTest.keyDown highC))) $
        through $ CmdTest.keyDown highC

    -- abort when a modifier is down
    let ckey = ctrlKey (Key.Char highC)
    equal (run (withKey (ctrlKey Key.ControlL)) (f True ckey)) (through ckey)

    equal (run st (f True (CmdTest.keyDown ';')))
        (through $ input (kbdNoteOn 60 4 0 0))
    equal (run st (f True (CmdTest.keyUp ';')))
        (through $ input (kbdNoteOff 60))

    let oct5 = st { Cmd.stateEdit = (Cmd.stateEdit st)
            { Cmd.stateKbdEntryOctave = 5 } }
    equal (run oct5 (f True (CmdTest.keyDown highC)))
        (through $ input (kbdNoteOn 84 6 0 0))
    -- with kbd_entry off, keys don't get mapped
    equal (run st (f False (CmdTest.keyDown highC)))
        (through (CmdTest.keyDown highC))

    -- test midi_entry (details tested in InputNote_test)
    equal (run st (f True (CmdTest.makeMidi (Midi.NoteOn 25 127))))
        (through $ input (CmdTest.noteOnNn 25))
    -- Doesn't require kbd_entry.
    equal (run st (f False (CmdTest.makeMidi (Midi.NoteOn 25 127))))
        (through $ input (CmdTest.noteOnNn 25))
    equal (run st (f False (CmdTest.makeMidi (Midi.NoteOff 25 127))))
        (through $ input (CmdTest.noteOff 25))

withKey :: Msg.Msg -> Cmd.State
withKey key = CmdTest.resultCmdState $
    CmdTest.run Ui.empty CmdTest.defaultCmdState (Internal.recordKeys key)

kbdNoteOn :: Int -> Pitch.Octave -> Pitch.PitchClass -> Pitch.Accidentals
    -> InputNote.Input
kbdNoteOn noteId oct pc accs =
    InputNote.NoteOn (InputNote.NoteId noteId)
        (Pitch.Input Pitch.AsciiKbd (CmdTest.pitch oct pc accs) 0)
        InputNote.keyboardVelocity

kbdNoteOff :: Int -> InputNote.GenericInput a
kbdNoteOff noteId = InputNote.NoteOff (InputNote.NoteId noteId)
    InputNote.keyboardVelocity
