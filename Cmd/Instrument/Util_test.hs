module Cmd.Instrument.Util_test where
import Util.Test

import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Instrument.Util as Util


test_keymaps = do
    let f = Util.keymaps [('a', "anote", 1), ('b', "bnote", 2)]
        empty = [(">", [])]
        run = run_sel empty
        msg = Midi.ChannelMessage 0
    -- NoEdit means midi but no note.
    equal (run False (f (CmdTest.key_down 'a')))
        (Right empty, [msg (Midi.NoteOn 1 64)])
    equal (run True (f (CmdTest.key_down 'a')))
        (Right [(">", [(0, 1, "anote")])], [msg (Midi.NoteOn 1 64)])
    equal (run True (f (CmdTest.key_up 'a')))
        (Right empty, [msg (Midi.NoteOff 1 64)])

run_sel tracks val_edit cmd = extract $ CmdTest.run_sel 0 tracks $ do
    Cmd.modify_edit_state $ \st -> st
        { Cmd.state_edit_mode = if val_edit then Cmd.ValEdit else Cmd.NoEdit
        , Cmd.state_kbd_entry = True
        }
    cmd

extract res = (CmdTest.e_tracks res, map snd (CmdTest.result_midi res))
